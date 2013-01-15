/* gstvoiceprint.c: oscilloscope+voiceprint

 * Copyright (C) <2002> Richard Boulton <richard@tartarus.org>
 * Copyright (C) <2006> Tim-Philipp Müller <tim centricular net>
 * Copyright (C) <2006> Wim Taymans <wim at fluendo dot com>
 * Copyright (C) <2012> Valentin But <valentin.but@eesti.ee>
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or(at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 */

// #####################################################################

/**
 * SECTION:element-voiceprint
 * @see_also: goom
 *
 * Voiceprint is an audio visualisation element. It creates a coloured
 * curve of the audio signal like on an oscilloscope and draws
 * voiceprint (discrete fourier transform) of the signal.
 *
 * <refsect2>
 * <title>Example launch line</title>
 * |[
 * gst-launch-1.0 -v audiotestsrc ! audioconvert ! voiceprint ! videoconvert ! ximagesink
 * ]|
 * </refsect2>
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <gst/video/video.h>
#include <gst/audio/audio.h>
#include <string.h>
#include "gstvoiceprint.h"
#include "voiceprint.h"

#define GST_PACKAGE_NAME    "Voiceprint"
#define GST_PACKAGE_ORIGIN  "http://pik-pik.ee/"

#define STRINGIFY(x) #x
#define TOSTRING(x) STRINGIFY(x)

#define VOICEPRINT_WIDTH    1600 //1280
#define VOICEPRINT_HEIGHT   900 //720
#define VOICEPRINT_FPS      25

GST_DEBUG_CATEGORY_STATIC(voiceprint_debug);
#define GST_CAT_DEFAULT voiceprint_debug

static GstStaticPadTemplate src_template = GST_STATIC_PAD_TEMPLATE("src",
    GST_PAD_SRC,
    GST_PAD_ALWAYS,
#if G_BYTE_ORDER == G_BIG_ENDIAN
    GST_STATIC_CAPS("video/x-raw, "
        "format =(string) xRGB, "
        "width = " TOSTRING(VOICEPRINT_WIDTH) ", " "height = " TOSTRING(VOICEPRINT_HEIGHT) ", " "framerate = " GST_VIDEO_FPS_RANGE)
#else
    GST_STATIC_CAPS("video/x-raw, "
        "format =(string) BGRx, "
        "width = " TOSTRING(VOICEPRINT_WIDTH) ", " "height = " TOSTRING(VOICEPRINT_HEIGHT) ", " "framerate = " GST_VIDEO_FPS_RANGE)
#endif
    );

static GstStaticPadTemplate sink_template = GST_STATIC_PAD_TEMPLATE("sink",
    GST_PAD_SINK,
    GST_PAD_ALWAYS,
    GST_STATIC_CAPS("audio/x-raw, "
        "format =(string) " GST_AUDIO_NE(S16) ", "
        "rate =(int) [ 8000, 96000 ], "
        "channels =(int) 1, " "layout =(string) interleaved")
    );


// #####################################################################

#define gst_voiceprint_parent_class parent_class
G_DEFINE_TYPE(GstVoiceprint, gst_voiceprint, GST_TYPE_ELEMENT);

// #####################################################################

static void gst_voiceprint_finalize(GObject *object);
static GstFlowReturn gst_voiceprint_chain(GstPad *pad, GstObject *parent, GstBuffer *buf);
static gboolean gst_voiceprint_src_setcaps(GstVoiceprint *mono, GstCaps *caps);
static gboolean gst_voiceprint_sink_setcaps(GstVoiceprint *mono, GstCaps *caps);
static void gst_voiceprint_reset(GstVoiceprint *voiceprint);
static gboolean gst_voiceprint_sink_event(GstPad *pad, GstObject *parent, GstEvent *event);
static gboolean gst_voiceprint_src_event(GstPad *pad, GstObject *parent, GstEvent *event);
static GstStateChangeReturn gst_voiceprint_change_state(GstElement *element, GstStateChange transition);

// #####################################################################
//
// INITIALIZATION AND DEINITIALIZATION
//

static void gst_voiceprint_class_init(GstVoiceprintClass *klass)
{
    GObjectClass *gobject_class;
    GstElementClass *gstelement_class;

    gobject_class =(GObjectClass *) klass;
    gstelement_class =(GstElementClass *) klass;

    gobject_class->finalize = gst_voiceprint_finalize;

    gstelement_class->change_state = GST_DEBUG_FUNCPTR(gst_voiceprint_change_state);

    gst_element_class_add_pad_template(gstelement_class, gst_static_pad_template_get(&src_template));
    gst_element_class_add_pad_template(gstelement_class,  gst_static_pad_template_get(&sink_template));
    gst_element_class_set_static_metadata(gstelement_class,
      "Voiceprint",
      "Visualization",
      "Displays scope and voiceprint",
      "Valentin But <valentin.but@eesti.ee");
}

static void gst_voiceprint_init(GstVoiceprint *voiceprint)
{
    voiceprint->sinkpad = gst_pad_new_from_static_template(&sink_template, "sink");
    gst_pad_set_chain_function(voiceprint->sinkpad, GST_DEBUG_FUNCPTR(gst_voiceprint_chain));
    gst_pad_set_event_function(voiceprint->sinkpad, GST_DEBUG_FUNCPTR(gst_voiceprint_sink_event));
    gst_element_add_pad(GST_ELEMENT(voiceprint), voiceprint->sinkpad);

    voiceprint->srcpad = gst_pad_new_from_static_template(&src_template, "src");
    gst_pad_set_event_function(voiceprint->srcpad, GST_DEBUG_FUNCPTR(gst_voiceprint_src_event));
    gst_element_add_pad(GST_ELEMENT(voiceprint), voiceprint->srcpad);

    voiceprint->adapter = gst_adapter_new();
    voiceprint->next_ts = GST_CLOCK_TIME_NONE;
    voiceprint->bps = sizeof(gint16);

    // reset the initial video state
    voiceprint->width = VOICEPRINT_WIDTH;
    voiceprint->height = VOICEPRINT_HEIGHT;
    voiceprint->fps_num = VOICEPRINT_FPS;      // desired frame rate
    voiceprint->fps_denom = 1;
    voiceprint->visstate = NULL;

    // reset the initial audio state
    voiceprint->rate = GST_AUDIO_DEF_RATE;
}

static void gst_voiceprint_finalize(GObject *object)
{
    GstVoiceprint *voiceprint = GST_VOICEPRINT(object);

    if(voiceprint->visstate)
        voiceprint_close(voiceprint->visstate);

    g_object_unref(voiceprint->adapter);

    G_OBJECT_CLASS(parent_class)->finalize(object);
}

static void gst_voiceprint_reset(GstVoiceprint * voiceprint)
{
    voiceprint->next_ts = GST_CLOCK_TIME_NONE;

    gst_adapter_clear(voiceprint->adapter);
    gst_segment_init(&voiceprint->segment, GST_FORMAT_UNDEFINED);

    GST_OBJECT_LOCK(voiceprint);
    voiceprint->proportion = 1.0;
    voiceprint->earliest_time = -1;
    GST_OBJECT_UNLOCK(voiceprint);
}

static gboolean gst_voiceprint_sink_setcaps(GstVoiceprint *voiceprint, GstCaps *caps)
{
    GstStructure *structure;

    structure = gst_caps_get_structure(caps, 0);

    gst_structure_get_int(structure, "rate", &voiceprint->rate);

    GST_DEBUG_OBJECT(voiceprint, "sample rate = %d", voiceprint->rate);

    return TRUE;
}

static gboolean gst_voiceprint_src_setcaps(GstVoiceprint *voiceprint, GstCaps * caps)
{
    GstStructure *structure;
    gboolean res;

    structure = gst_caps_get_structure(caps, 0);

    gst_structure_get_int(structure, "width", &voiceprint->width);
    gst_structure_get_int(structure, "height", &voiceprint->height);
    gst_structure_get_fraction(structure, "framerate", &voiceprint->fps_num, &voiceprint->fps_denom);

    voiceprint->outsize = voiceprint->width * voiceprint->height * sizeof(guint32);
    voiceprint->frame_duration = gst_util_uint64_scale_int(GST_SECOND, voiceprint->fps_denom, voiceprint->fps_num);
    voiceprint->spf = gst_util_uint64_scale_int(voiceprint->rate, voiceprint->fps_denom, voiceprint->fps_num);

    GST_DEBUG_OBJECT(voiceprint, "dimension %dx%d, framerate %d/%d, spf %d",
        voiceprint->width, voiceprint->height, voiceprint->fps_num,
        voiceprint->fps_denom, voiceprint->spf);

    if(voiceprint->visstate)
    {
        voiceprint_close(voiceprint->visstate);
        voiceprint->visstate = NULL;
    }

    voiceprint->visstate = voiceprint_init(voiceprint->width, voiceprint->height, voiceprint->fps_denom, voiceprint->fps_num, voiceprint->rate, voiceprint->spf );

    res = gst_pad_set_caps(voiceprint->srcpad, caps);

    return res &&(voiceprint->visstate != NULL);
}

// #####################################################################

static gboolean gst_voiceprint_src_negotiate(GstVoiceprint * voiceprint)
{
    GstCaps *othercaps, *target;
    GstStructure *structure;
    GstCaps *templ;
    GstQuery *query;
    GstBufferPool *pool;
    GstStructure *config;
    guint size, min, max;

    templ = gst_pad_get_pad_template_caps(voiceprint->srcpad);

    GST_DEBUG_OBJECT(voiceprint, "performing negotiation");

    // See what the peer can do
    othercaps = gst_pad_peer_query_caps( voiceprint->srcpad, NULL );
    if(othercaps)
    {
        target = gst_caps_intersect(othercaps, templ);
        gst_caps_unref(othercaps);
        gst_caps_unref(templ);

        if(gst_caps_is_empty(target))
            goto no_format;

        target = gst_caps_truncate(target);
    }
    else
    {
        target = templ;
    }

    target = gst_caps_make_writable(target);
    structure = gst_caps_get_structure(target, 0);
    gst_structure_fixate_field_nearest_int(structure, "width", 320);
    gst_structure_fixate_field_nearest_int(structure, "height", 240);
    gst_structure_fixate_field_nearest_fraction(structure, "framerate", 25, 1);

    gst_voiceprint_src_setcaps(voiceprint, target);

    // try to get a bufferpool now
    // find a pool for the negotiated caps now
    query = gst_query_new_allocation(target, TRUE);

    if(!gst_pad_peer_query(voiceprint->srcpad, query))
    {
    }

    if(gst_query_get_n_allocation_pools(query) > 0)
    {
        // we got configuration from our peer, parse them */
        gst_query_parse_nth_allocation_pool(query, 0, &pool, &size, &min, &max);
    }
    else
    {
        pool = NULL;
        size = voiceprint->outsize;
        min = max = 0;
    }

    if(pool == NULL)
    {
        // we did not get a pool, make one ourselves then
        pool = gst_buffer_pool_new();
    }

    config = gst_buffer_pool_get_config(pool);
    gst_buffer_pool_config_set_params(config, target, size, min, max);
    gst_buffer_pool_set_config(pool, config);

    if(voiceprint->pool)
    {
        gst_buffer_pool_set_active(voiceprint->pool, TRUE);
        gst_object_unref(voiceprint->pool);
    }
    voiceprint->pool = pool;

    // and activate
    gst_buffer_pool_set_active(pool, TRUE);

    gst_caps_unref(target);

    return TRUE;

no_format:
    {
        gst_caps_unref(target);
        return FALSE;
    }
}

// make sure we are negotiated
static GstFlowReturn ensure_negotiated(GstVoiceprint * voiceprint)
{
    gboolean reconfigure;

    reconfigure = gst_pad_check_reconfigure(voiceprint->srcpad);

    // we don't know an output format yet, pick one
    if(reconfigure || !gst_pad_has_current_caps(voiceprint->srcpad))
    {
        if(!gst_voiceprint_src_negotiate(voiceprint))
            return GST_FLOW_NOT_NEGOTIATED;
    }
    return GST_FLOW_OK;
}

// #####################################################################

static GstFlowReturn gst_voiceprint_chain(GstPad * pad, GstObject * parent, GstBuffer * inbuf)
{
    GstFlowReturn flow_ret = GST_FLOW_OK;
    GstVoiceprint *voiceprint;

    voiceprint = GST_VOICEPRINT(parent);

    if(voiceprint->rate == 0)
    {
        gst_buffer_unref(inbuf);
        flow_ret = GST_FLOW_NOT_NEGOTIATED;
        goto out;
    }

    // Make sure have an output format
    flow_ret = ensure_negotiated(voiceprint);
    if(flow_ret != GST_FLOW_OK)
    {
        gst_buffer_unref(inbuf);
        goto out;
    }

    // don't try to combine samples from discont buffer
    if(GST_BUFFER_FLAG_IS_SET(inbuf, GST_BUFFER_FLAG_DISCONT))
    {
        gst_adapter_clear(voiceprint->adapter);
        voiceprint->next_ts = GST_CLOCK_TIME_NONE;
    }

    // Match timestamps from the incoming audio
    if(GST_BUFFER_TIMESTAMP(inbuf) != GST_CLOCK_TIME_NONE)
        voiceprint->next_ts = GST_BUFFER_TIMESTAMP(inbuf);

    GST_LOG_OBJECT(voiceprint, "in buffer has %d samples, ts=%" GST_TIME_FORMAT,
        gst_buffer_get_size(inbuf) / voiceprint->bps,
        GST_TIME_ARGS(GST_BUFFER_TIMESTAMP(inbuf)));

    gst_adapter_push(voiceprint->adapter, inbuf);
    inbuf = NULL;

    // Collect samples until we have enough for an output frame
    while(flow_ret == GST_FLOW_OK)
    {
        gint16 *samples;
        GstBuffer *outbuf = NULL;
        guint32 *pixels, avail, bytesperframe;

        avail = gst_adapter_available(voiceprint->adapter);
        GST_LOG_OBJECT(voiceprint, "bytes avail now %u", avail);

        bytesperframe = voiceprint->spf * voiceprint->bps;
        if(avail < bytesperframe)
            break;

        samples = (gint16 *) gst_adapter_map(voiceprint->adapter, bytesperframe);

        /*if( voiceprint->spf < voiceprint->spf )
        {
            gint16 in_data[voiceprint->spf], i;

            for(i = 0; i < voiceprint->spf; ++i) {
                gdouble off;

                off = ((gdouble) i *(gdouble) voiceprint->spf) / (gdouble) voiceprint->spf;
                in_data[i] = samples[MIN((guint) off, voiceprint->spf)];
            }
            
            pixels = voiceprint_update(voiceprint->visstate, in_data);
        }
        else
        {
            // not really correct, but looks much prettier
            pixels = voiceprint_update(voiceprint->visstate, samples);
        }*/
        
        pixels = voiceprint_update(voiceprint->visstate, samples);

        GST_LOG_OBJECT(voiceprint, "allocating output buffer");
        flow_ret = gst_buffer_pool_acquire_buffer(voiceprint->pool, &outbuf, NULL);
        
        if(flow_ret != GST_FLOW_OK)
        {
            gst_adapter_unmap(voiceprint->adapter);
            goto out;
        }

        gst_buffer_fill(outbuf, 0, pixels, voiceprint->outsize);

        GST_BUFFER_TIMESTAMP(outbuf) = voiceprint->next_ts;
        GST_BUFFER_DURATION(outbuf) = voiceprint->frame_duration;

        flow_ret = gst_pad_push(voiceprint->srcpad, outbuf);

        if(GST_CLOCK_TIME_IS_VALID(voiceprint->next_ts))
            voiceprint->next_ts += voiceprint->frame_duration;

        gst_adapter_flush(voiceprint->adapter, bytesperframe);
    }

out:

    return flow_ret;
}

// #####################################################################

static gboolean gst_voiceprint_sink_event(GstPad * pad, GstObject * parent, GstEvent * event)
{
    GstVoiceprint *voiceprint;
    gboolean res;

    voiceprint = GST_VOICEPRINT(parent);

    switch(GST_EVENT_TYPE(event))
    {
        case GST_EVENT_FLUSH_START:
            res = gst_pad_push_event(voiceprint->srcpad, event);
            break;
            
        case GST_EVENT_FLUSH_STOP:
            gst_voiceprint_reset(voiceprint);
            res = gst_pad_push_event(voiceprint->srcpad, event);
            break;
            
        case GST_EVENT_SEGMENT:
        {
            // the newsegment values are used to clip the input samples
            // and to convert the incomming timestamps to running time so
            // we can do QoS
            gst_event_copy_segment(event, &voiceprint->segment);

            res = gst_pad_push_event(voiceprint->srcpad, event);
            break;
        }
        
        case GST_EVENT_CAPS:
        {
            GstCaps *caps;

            gst_event_parse_caps(event, &caps);
            gst_voiceprint_sink_setcaps(voiceprint, caps);
            gst_event_unref(event);
            res = TRUE;
            break;
        }
        
        default:
            res = gst_pad_push_event(voiceprint->srcpad, event);
            break;
    }

    return res;
}

// #####################################################################

static gboolean gst_voiceprint_src_event(GstPad * pad, GstObject * parent, GstEvent * event)
{
    GstVoiceprint *voiceprint;
    gboolean res;

    voiceprint = GST_VOICEPRINT(parent);

    switch(GST_EVENT_TYPE(event))
    {
        case GST_EVENT_QOS:
        {
            gdouble proportion;
            GstClockTimeDiff diff;
            GstClockTime timestamp;

            gst_event_parse_qos(event, NULL, &proportion, &diff, &timestamp);

            // save stuff for the _chain() function
            GST_OBJECT_LOCK(voiceprint);
            voiceprint->proportion = proportion;
            
            if(diff >= 0)
                // we're late, this is a good estimate for next displayable
                // frame(see part-qos.txt)
                voiceprint->earliest_time = timestamp + 2 * diff + voiceprint->frame_duration;
            else
                voiceprint->earliest_time = timestamp + diff;
            
            GST_OBJECT_UNLOCK(voiceprint);

            res = gst_pad_push_event(voiceprint->sinkpad, event);
            break;
        }
        
        default:
            res = gst_pad_push_event(voiceprint->sinkpad, event);
            break;
    }

    return res;
}

// #####################################################################

static GstStateChangeReturn gst_voiceprint_change_state(GstElement * element, GstStateChange transition)
{
    GstVoiceprint *voiceprint = GST_VOICEPRINT(element);
    GstStateChangeReturn ret;

    switch(transition)
    {
        case GST_STATE_CHANGE_NULL_TO_READY:
            break;
            
        case GST_STATE_CHANGE_READY_TO_PAUSED:
            gst_voiceprint_reset(voiceprint);
            break;
    
        default:
            break;
    }

    ret = GST_ELEMENT_CLASS(parent_class)->change_state(element, transition);

    switch(transition)
    {
        case GST_STATE_CHANGE_PAUSED_TO_READY:
            if(voiceprint->pool)
            {
                gst_buffer_pool_set_active(voiceprint->pool, FALSE);
                gst_object_replace((GstObject **) & voiceprint->pool, NULL);
            }
            break;
    
            case GST_STATE_CHANGE_READY_TO_NULL:
                break;
            
            default:
                break;
    }

    return ret;
}

// #####################################################################

static gboolean
plugin_init(GstPlugin *plugin)
{
    GST_DEBUG_CATEGORY_INIT(voiceprint_debug, "voiceprint", 0, "voiceprint element");

    return gst_element_register(plugin, "voiceprint", GST_RANK_NONE, GST_TYPE_VOICEPRINT);
}

GST_PLUGIN_DEFINE(
    GST_VERSION_MAJOR,
    GST_VERSION_MINOR,
    voiceprint,
    "Voiceprint visualization",
    plugin_init, VERSION, "LGPL", GST_PACKAGE_NAME, GST_PACKAGE_ORIGIN);

// #####################################################################
