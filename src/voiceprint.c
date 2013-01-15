/*  voiceprint.cpp
 *
 *  Copyright (C) 2012 Valentin But <valentin.but@eesti.ee>
 *
 * Originally it is from monoscope plugin in gstreamer.
 * I accidentally changed it beyond recognition.
 * 
 * The interface of this file consists of three functions:
 *      voiceprint_init,
 *      voiceprint_update,
 *      and voiceprint_close.
 * 
 * Initialization is done by voiceprint_init with desired parameters.
 * Samples per second should equal sample rate divided frame per
 * second:
 * 
 *      spf = rate / fps.
 * 
 * Then samples are supplied to voiceprint_update and image is
 * outputted in 32-bit format: 0x0xxRRGGBB.
 * 
 * To free used memory call voiceprint_close.
 * 
 * This file is under what is known as the BSD license:
 *
 * Redistribution and use in source and binary forms, with or without modification, i
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this list of 
 * conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice, this list 
 * of conditions and the following disclaimer in the documentation and/or other materials 
 * provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products derived from 
 * this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, 
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR 
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT 
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; 
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY 
 * WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

// #####################################################################

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "voiceprint.h"

#include <string.h>
#include <stdlib.h>
#include <pthread.h>
#include <stdbool.h>
#include <math.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

// #####################################################################
//
// INITIALIZATION AND DEINITIALIZATION
//

struct voiceprint_state *voiceprint_init( guint32 resx, guint32 resy, int fps_denom, int fps_num, int rate, int spf )
{
    struct voiceprint_state *stateptr;

    stateptr = calloc( 1, sizeof(struct voiceprint_state) );

    stateptr->display_width = resx;
    stateptr->display_height = resy;

    // This buffer stores the actual output image
    stateptr->display = malloc( (stateptr->display_width + 1) * (stateptr->display_height + 1) * sizeof(guint32) );
    
    // Save info
    stateptr->fps_denom = fps_denom;
    stateptr->fps_num = fps_num;
    stateptr->fps = (double) fps_num / (double) fps_denom;
    stateptr->rate = rate;
    stateptr->spf = spf; //(resy / 2 - 20) * 2;

    stateptr->fft_size = stateptr->spf;
    stateptr->fft_image_pos = 0;

    // Scope settings
    stateptr->scope_x = 10*3;
    stateptr->scope_y = 10;
    stateptr->scope_width = stateptr->display_width / 2 - stateptr->scope_x - 10;
    stateptr->scope_height = stateptr->display_height / 2 - stateptr->scope_y * 2;    
    stateptr->scope_color = 0x00000FF00;

    // Scope grid settings
    stateptr->scope_grid_color = 0x000FFFFFF;
    stateptr->scope_border_color = 0x000FFFFFF;
    stateptr->scope_axis_color = 0x000FFFFFF;
    stateptr->scope_grid_cols = 20;
    stateptr->scope_grid_rows = 10;
    stateptr->scope_grid_x = stateptr->scope_x;
    stateptr->scope_grid_y = stateptr->scope_y;
    stateptr->scope_grid_width = stateptr->scope_width;
    stateptr->scope_grid_height = stateptr->scope_height;

    // Voiceprint settings
    stateptr->voice_val_range_min = 0.0;
    stateptr->voice_val_range_max = 25.0;
    stateptr->voice_x = 10*4;
    stateptr->voice_y = 10 + stateptr->display_height / 2;
    stateptr->voice_width = stateptr->display_width - stateptr->voice_x - 10;
    stateptr->voice_height = stateptr->display_height / 2 - 20 - 3;

    // Voiceprint grid settings
    stateptr->voice_grid_color = 0x000FFFFFF;
    stateptr->voice_border_color = 0x000FFFFFF;
    stateptr->voice_axis_color = 0x000000000;
    stateptr->voice_grid_cols = 0;
    stateptr->voice_grid_rows = 0;
    stateptr->voice_grid_x = stateptr->voice_x;
    stateptr->voice_grid_y = stateptr->voice_y;
    stateptr->voice_grid_width = stateptr->voice_width;
    stateptr->voice_grid_height = stateptr->voice_height;

    // Spectrum settings
    stateptr->spectrum_val_range_min = stateptr->voice_val_range_min;
    stateptr->spectrum_val_range_max = stateptr->voice_val_range_max;
    stateptr->spectrum_x = 10*3 + stateptr->display_width / 2;
    stateptr->spectrum_y = 10;
    stateptr->spectrum_width = stateptr->display_width / 2 - 10*4;
    stateptr->spectrum_height = stateptr->display_height / 2 - stateptr->spectrum_y * 2;    
    stateptr->spectrum_color = 0x000FFFFFF;

    // Spectrum grid
    stateptr->spectrum_grid_color = 0x000FFFFFF;
    stateptr->spectrum_border_color = 0x000FFFFFF;
    stateptr->spectrum_axis_color = 0x000000000;
    stateptr->spectrum_grid_cols = 0;
    stateptr->spectrum_grid_rows = 0;
    stateptr->spectrum_grid_x = stateptr->spectrum_x;
    stateptr->spectrum_grid_y = stateptr->spectrum_y;
    stateptr->spectrum_grid_width = stateptr->spectrum_width;
    stateptr->spectrum_grid_height = stateptr->spectrum_height;

    //
    // Sound level settings
    //
    // Very fine selection of coefficients.
    // Slightest change may result in instability and it will not work.
    //
    
    // Stiffness of the system. Do not change, always 1.0
    // If changed, then established meaning will be different from
    // the input value proportionally.
    const double k = 1.0;
    
    // Increse these together makes slower response and decreasing
    // is vice-versa.
    const double c = 0.05;           // Viscous damping coefficient
    stateptr->spring.m = 0.003;      // Mass
    
    // Keep damping ratio somewhere between 1.0 ... 0.5
    // The output may oscillate uncontrolled, when solving numerically
    // with values outside this range. Despite the fact the analytic
    // solution is fine.
    stateptr->spring.zeta = c / ( 2 * sqrt( stateptr->spring.m * k ) ); // Damping ratio

    // Let it calculate the right value.
    stateptr->spring.omega = sqrt( k / stateptr->spring.m );            // Undamped angular frequency of the oscillator
    
    // Zero initial state.
    stateptr->spring.x[0] = 0.0;
    stateptr->spring.z[0] = 0.0;

    //
    // Prepare buffer for image
    //
    stateptr->fft_image_width = stateptr->voice_width;
    stateptr->fft_image_size = stateptr->fft_image_width * stateptr->fft_size * sizeof(guint32);
    stateptr->fft_data_size = stateptr->fft_image_width * stateptr->fft_size * sizeof(double);
    stateptr->fft_image = malloc( stateptr->fft_image_size );
    stateptr->fft_data = malloc( stateptr->fft_data_size );

    // Allocate the input and output arrays
    stateptr->fft_in = (double *) fftw_malloc( sizeof(double) * stateptr->fft_size );
    stateptr->fft_out = (fftw_complex *) fftw_malloc( sizeof(fftw_complex) * stateptr->fft_size );
    stateptr->p = fftw_plan_dft_r2c_1d( stateptr->fft_size, stateptr->fft_in, stateptr->fft_out, FFTW_ESTIMATE );

    return stateptr;
}

void voiceprint_close( struct voiceprint_state *stateptr )
{
    free( stateptr->fft_image );
    free( stateptr->fft_data );
    fftw_destroy_plan( stateptr->p );
    fftw_free( stateptr->fft_in );
    fftw_free( stateptr->fft_out );

    free( stateptr->display );

    free( stateptr );
}

// #####################################################################

char *make_message( const char *fmt, ... )
{
    int size = 100;  // Guess we need no more than 100 bytes.

	char *p = malloc( size );
    if( p == NULL )
        return NULL;

    while( 1 )
    {
        // Try to print in the allocated space.
        va_list ap;
        va_start( ap, fmt );
        int n = vsnprintf( p, size, fmt, ap );
        va_end( ap );

        // If that worked, return the string.
        if (n > -1 && n < size)
            return p;

        // Else try again with more space.
        if (n > -1)     // glibc 2.1
            size = n+1; // precisely what is needed
        else            // glibc 2.0
            size *= 2;  // twice the old size

        char *np = realloc( p, size );
        if( np == NULL )
        {
            free( p );
            return NULL;
        }
        else
        {
            p = np;
        }
    }
}

// #####################################################################
//
// TEXT DRAW
//
// Modified code of EZDIB. Original license:
//
// Copyright (c) 1997 - 2012
// Robert Umbehant
// ezdib@wheresjames.com
// http://www.wheresjames.com
//
// Redistribution and use in source and binary forms, with or
// without modification, are permitted for commercial and
// non-commercial purposes, provided that the following
// conditions are met:
//
// * Redistributions of source code must retain the above copyright
//   notice, this list of conditions and the following disclaimer.
// * The names of the developers or contributors may not be used to
//   endorse or promote products derived from this software without
//   specific prior written permission.

// A medium font map
static const unsigned char font_map_medium [] =
{
	// Default glyph
	'.', 2, 10,	0x00, 0x3c, 0x00,

	// Tab width
	'\t', 10, 0,

	// Space
	' ', 2, 0,

	'!', 1, 10,	0xf6, 0x00,
	'(', 3, 10,	0x2a, 0x48, 0x88, 0x00,
	')', 3, 10,	0x88, 0x92, 0xa0, 0x00,
	',', 2, 10,	0x00, 0x16, 0x00,
	'-', 3, 10,	0x00, 0x70, 0x00, 0x00,
	'/', 3, 10,	0x25, 0x25, 0x20, 0x00,
	'@', 6, 10,	0x7a, 0x19, 0x6b, 0x9a, 0x07, 0x80, 0x00, 0x00,
	'$', 5, 10,	0x23, 0xab, 0x47, 0x16, 0xae, 0x20, 0x00,
	'#', 6, 10,	0x49, 0x2f, 0xd2, 0xfd, 0x24, 0x80, 0x00, 0x00,
	'%', 7, 10,	0x43, 0x49, 0x20, 0x82, 0x49, 0x61, 0x00, 0x00, 0x00,
	':', 2, 10,	0x3c, 0xf0, 0x00,
	'^', 3, 10,	0x54, 0x00, 0x00, 0x00,
	'~', 5, 10,	0x00, 0x11, 0x51, 0x00, 0x00, 0x00, 0x00,

	'0', 5, 10,	0x74, 0x73, 0x59, 0xc5, 0xc0, 0x00, 0x00,
	'1', 3, 10,	0xc9, 0x24, 0xb8, 0x00,
	'2', 5, 10,	0x74, 0x42, 0xe8, 0x43, 0xe0, 0x00, 0x00,
	'3', 5, 10,	0x74, 0x42, 0xe0, 0xc5, 0xc0, 0x00, 0x00,
	'4', 5, 10,	0x11, 0x95, 0x2f, 0x88, 0x40, 0x00, 0x00,
	'5', 5, 10,	0xfc, 0x3c, 0x10, 0xc5, 0xc0, 0x00, 0x00,
	'6', 5, 10,	0x74, 0x61, 0xe8, 0xc5, 0xc0, 0x00, 0x00,
	'7', 5, 10,	0xfc, 0x44, 0x42, 0x10, 0x80, 0x00, 0x00,
	'8', 5, 10,	0x74, 0x62, 0xe8, 0xc5, 0xc0, 0x00, 0x00,
	'9', 5, 10,	0x74, 0x62, 0xf0, 0xc5, 0xc0, 0x00, 0x00,

	'A', 6, 10,	0x31, 0x28, 0x7f, 0x86, 0x18, 0x40, 0x00, 0x00,
	'B', 6, 10,	0xfa, 0x18, 0x7e, 0x86, 0x1f, 0x80, 0x00, 0x00,
	'C', 6, 10,	0x7a, 0x18, 0x20, 0x82, 0x17, 0x80, 0x00, 0x00,
	'D', 6, 10,	0xfa, 0x18, 0x61, 0x86, 0x1f, 0x80, 0x00, 0x00,
	'E', 6, 10,	0xfe, 0x08, 0x3c, 0x82, 0x0f, 0xc0, 0x00, 0x00,
	'F', 6, 10,	0xfe, 0x08, 0x3c, 0x82, 0x08, 0x00, 0x00, 0x00,
	'G', 6, 10,	0x7a, 0x18, 0x27, 0x86, 0x17, 0xc0, 0x00, 0x00,
	'H', 6, 10,	0x86, 0x18, 0x7f, 0x86, 0x18, 0x40, 0x00, 0x00,
	'I', 3, 10,	0xe9, 0x24, 0xb8, 0x00,
	'J', 6, 10,	0xfc, 0x41, 0x04, 0x12, 0x46, 0x00, 0x00, 0x00,
	'K', 5, 10,	0x8c, 0xa9, 0x8a, 0x4a, 0x20, 0x00, 0x00,
	'L', 4, 10,	0x88, 0x88, 0x88, 0xf0, 0x00,
	'M', 6, 10,	0x87, 0x3b, 0x61, 0x86, 0x18, 0x40, 0x00, 0x00,
	'N', 5, 10,	0x8e, 0x6b, 0x38, 0xc6, 0x20, 0x00, 0x00,
	'O', 6, 10,	0x7a, 0x18, 0x61, 0x86, 0x17, 0x80, 0x00, 0x00,
	'P', 5, 10,	0xf4, 0x63, 0xe8, 0x42, 0x00, 0x00, 0x00,
	'Q', 6, 10,	0x7a, 0x18, 0x61, 0x86, 0x57, 0x81, 0x00, 0x00,
	'R', 5, 10,	0xf4, 0x63, 0xe8, 0xc6, 0x20, 0x00, 0x00,
	'S', 6, 10,	0x7a, 0x18, 0x1e, 0x06, 0x17, 0x80, 0x00, 0x00,
	'T', 3, 10,	0xe9, 0x24, 0x90, 0x00,
	'U', 6, 10,	0x86, 0x18, 0x61, 0x86, 0x17, 0x80, 0x00, 0x00,
	'V', 6, 10,	0x86, 0x18, 0x61, 0x85, 0x23, 0x00, 0x00, 0x00,
	'W', 7, 10,	0x83, 0x06, 0x4c, 0x99, 0x35, 0x51, 0x00, 0x00, 0x00,
	'X', 5, 10,	0x8c, 0x54, 0x45, 0x46, 0x20, 0x00, 0x00,
	'Y', 5, 10,	0x8c, 0x54, 0x42, 0x10, 0x80, 0x00, 0x00,
	'Z', 6, 10,	0xfc, 0x10, 0x84, 0x21, 0x0f, 0xc0, 0x00, 0x00,

	'a', 4, 10,	0x00, 0x61, 0x79, 0x70, 0x00,
	'b', 4, 10,	0x88, 0xe9, 0x99, 0xe0, 0x00,
	'c', 4, 10,	0x00, 0x78, 0x88, 0x70, 0x00,
	'd', 4, 10,	0x11, 0x79, 0x99, 0x70, 0x00,
	'e', 4, 10,	0x00, 0x69, 0xf8, 0x60, 0x00,
	'f', 4, 10,	0x25, 0x4e, 0x44, 0x40, 0x00,
	'g', 4, 10,	0x00, 0x79, 0x99, 0x71, 0x60,
	'h', 4, 10,	0x88, 0xe9, 0x99, 0x90, 0x00,
	'i', 1, 10,	0xbe, 0x00,
	'j', 2, 10,	0x04, 0x55, 0x80,
	'k', 4, 10,	0x89, 0xac, 0xca, 0x90, 0x00,
	'l', 3, 10,	0xc9, 0x24, 0x98, 0x00,
	'm', 5, 10,	0x00, 0x15, 0x5a, 0xd6, 0x20, 0x00, 0x00,
	'n', 4, 10,	0x00, 0xe9, 0x99, 0x90, 0x00,
	'o', 4, 10,	0x00, 0x69, 0x99, 0x60, 0x00,
	'p', 4, 10,	0x00, 0xe9, 0x99, 0xe8, 0x80,
	'q', 4, 10,	0x00, 0x79, 0x97, 0x11, 0x10,
	'r', 3, 10,	0x02, 0xe9, 0x20, 0x00,
	's', 4, 10,	0x00, 0x78, 0x61, 0xe0, 0x00,
	't', 3, 10,	0x4b, 0xa4, 0x88, 0x00,
	'u', 4, 10,	0x00, 0x99, 0x99, 0x70, 0x00,
	'v', 4, 10,	0x00, 0x99, 0x99, 0x60, 0x00,
	'w', 5, 10,	0x00, 0x23, 0x1a, 0xd5, 0x40, 0x00, 0x00,
	'x', 5, 10,	0x00, 0x22, 0xa2, 0x2a, 0x20, 0x00, 0x00,
	'y', 4, 10,	0x00, 0x99, 0x99, 0x71, 0x60,
	'z', 4, 10,	0x00, 0xf1, 0x24, 0xf0, 0x00,

	0,

};

static void ezd_draw_bmp_32( struct voiceprint_state *stateptr, guint32 *pImg, int bw, int bh, const unsigned char *pBmp, guint32 col )
{
	unsigned char m = 0x80;

	// Draw the glyph
	for( int h = 0; h < bh; h++ )
	{
		// Draw horz line
		for( int w = 0; w < bw; w++ )
		{
			// Next glyph byte?
			if ( !m )
				m = 0x80, pBmp++;

			// Is this pixel on?
			if ( *pBmp & m )
				*pImg = col;

			// Next bmp bit
			m >>= 1;

			// Next pixel
			pImg++;

		} // end for

		// Next image line
		pImg += stateptr->display_width - bw;

	} // end for

}

const unsigned char *ezd_next_glyph( const unsigned char *pGlyph )
{
	// Last glyph?
	if ( !pGlyph || !*pGlyph )
		return 0;

	// Glyph size in bits
	int sz = pGlyph[ 1 ] * pGlyph[ 2 ];

	// Return a pointer to the next glyph
	return &pGlyph[ 3 + ( ( sz & 0x07 ) ? ( ( sz >> 3 ) + 1 ) : sz >> 3 ) ];
}

const unsigned char *ezd_find_glyph( const unsigned char *x_pFt, const char ch )
{
	const unsigned char* pGlyph = x_pFt;

	// Find the glyph
	while ( pGlyph && *pGlyph )
		if ( ch == *pGlyph )
			return pGlyph;
		else
			pGlyph = ezd_next_glyph( pGlyph );

	// First glyph is the default
	return x_pFt;
}

int ezd_text( struct voiceprint_state *stateptr, const char *x_pText, int x_nTextLen, int x, int y, guint32 x_col )
{
    guint32 *pImage = stateptr->display;

	// For each character in the string
    int mh = 0; int lx = x;
	for ( int i = 0; i < x_nTextLen || ( 0 > x_nTextLen && x_pText[ i ] ); i++ )
	{
		// Get the specified glyph
		const unsigned char *pGlyph = ezd_find_glyph( font_map_medium, x_pText[ i ] );

		// CR, just go back to starting x pos
		if ( '\r' == x_pText[ i ] )
			lx = x;

		// LF - Back to starting x and next line
		else if ( '\n' == x_pText[ i ] )
			lx = x, y += 1 + mh, mh = 0;

		// Other characters
		else
		{
			// Draw this glyph if it's completely on the screen
			if ( pGlyph[ 1 ] && pGlyph[ 2 ]
				 && 0 <= lx && ( lx + pGlyph[ 1 ] ) < stateptr->display_width
				 && 0 <= y && ( y + pGlyph[ 2 ] ) < stateptr->display_height )
			{
                ezd_draw_bmp_32( stateptr, &pImage[ y * stateptr->display_width + lx ], pGlyph[ 1 ], pGlyph[ 2 ], &pGlyph[ 3 ], x_col );

			} // end if

			// Next character position
			lx += 2 + pGlyph[ 1 ];

			// Track max height
			mh = ( pGlyph[ 2 ] > mh ) ? pGlyph[ 2 ] : mh;

		} // end else

	} // end for

	return 1;
}

int ezd_text_centered( struct voiceprint_state *stateptr, const char *x_pText, int x_nTextLen, int x, int y, guint32 x_col )
{
    int lx = 0;
	for ( int i = 0; i < x_nTextLen || ( 0 > x_nTextLen && x_pText[ i ] ); i++ )
	{
		// Get the specified glyph
		const unsigned char *pGlyph = ezd_find_glyph( font_map_medium, x_pText[ i ] );

        // Next character position
        lx += 2 + pGlyph[ 1 ];

	} // end for

    return ezd_text( stateptr, x_pText, x_nTextLen, x - lx / 2, y, x_col );
}

int ezd_text_right( struct voiceprint_state *stateptr, const char *x_pText, int x_nTextLen, int x, int y, guint32 x_col )
{
    int lx = 0;
	for ( int i = 0; i < x_nTextLen || ( 0 > x_nTextLen && x_pText[ i ] ); i++ )
	{
		// Get the specified glyph
		const unsigned char *pGlyph = ezd_find_glyph( font_map_medium, x_pText[ i ] );

        // Next character position
        lx += 2 + pGlyph[ 1 ];

	} // end for

    return ezd_text( stateptr, x_pText, x_nTextLen, x - lx, y, x_col );
}

// #####################################################################
//
// MATH FUNCTIONS
//

void limit_vald( double *val, double min, double max )
{
    if( *val < min )
        *val = min;
    else if( *val > max )
        *val = max;
}

// #####################################################################

// Declare the colormap
struct colormap_t
{
    double val;
    double r;
    double g;
    double b;
};

// Hot colormap
struct colormap_t map_hot[] =
{
    { 0.0, 0.0, 0.0, 0.0 },
    { 0.3, 1.0, 0.0, 0.0 },
    { 0.6, 1.0, 1.0, 0.0 },
    { 1.0, 1.0, 1.0, 1.0 }
};

// Jet colormap
struct colormap_t map_jet[] =
{
    { 0.0, 0.0, 0.0, 0.5 },
    { 0.375, 0.0, 1.0, 1.0 },
    { 0.625, 1.0, 1.0, 0.0 },
    { 0.875, 1.0, 0.0, 0.0 },
    { 1.0, 0.5, 0.0, 0.0 }
};

void colormap( double val, double min, double max,
    unsigned char *r, unsigned char *g, unsigned char *b )
{
    struct colormap_t *map = map_hot;
    const int map_n = sizeof(map_hot) / sizeof(struct colormap_t);

    // Normalise the value
    double val_norm = (val - min) / (max - min);

    // Find interal in the colormap
    int i;
    for( i = 0; i < map_n - 1; i++ )
    {
        if( map[i].val <= val_norm &&
            map[i + 1].val >= val_norm )
        {
            // Found
            goto colormap_found;
        }
    }

    // Not found
    if( map[0].val > val_norm )
    {
        *r = map[0].r * 255.0;
        *g = map[0].g * 255.0;
        *b = map[0].b * 255.0;
    }
    else if( map[map_n - 1].val < val_norm )
    {
        *r = map[map_n - 1].r * 255.0;
        *g = map[map_n - 1].g * 255.0;
        *b = map[map_n - 1].b * 255.0;
    }
    
    return;

    double val_pos;

colormap_found:

    // Calculate the relative position of the value in the interval
    val_pos = (val_norm - map[i].val) / (map[i + 1].val - map[i].val);

    // Interpolate color component according to val_pos
    double rd = val_pos * (map[i + 1].r - map[i].r) + map[i].r;
    double gd = val_pos * (map[i + 1].g - map[i].g) + map[i].g;
    double bd = val_pos * (map[i + 1].b - map[i].b) + map[i].b;

    // Output
    *r = rd * 255.0;
    *g = gd * 255.0;
    *b = bd * 255.0;

    //printf( "val=%g, val_norm=%g, val_pos=%g, i=%i, rgb = %g %g %g\n", val, val_norm, val_pos, i, rd, gd, bd );
}

// #####################################################################

void blend_color( guint32 *dst, guint32 color, double factor )
{
    // Get color elements
    double dst_r = 0x0FF & (*dst >> (8 * 2));
    double dst_g = 0x0FF & (*dst >> (8 * 1));
    double dst_b = 0x0FF & (*dst >> (8 * 0));
    double color_r = 0x0FF & (color >> (8 * 2));
    double color_g = 0x0FF & (color >> (8 * 1));
    double color_b = 0x0FF & (color >> (8 * 0));
    
    // Blend
    double out_r = dst_r + (color_r - dst_r) * factor;
    double out_g = dst_g + (color_g - dst_g) * factor;
    double out_b = dst_b + (color_b - dst_b) * factor;
    
    // Output
    *dst =  (guint32) out_r << (8 * 2);
    *dst |= (guint32) out_g << (8 * 1);
    *dst |= (guint32) out_b << (8 * 0);
}

// #####################################################################

void draw_pixel( struct voiceprint_state *stateptr, int x, int y, guint32 color, double factor  )
{
    if( x >= stateptr->display_width || y >= stateptr->display_height || x < 0 || y < 0 )
        return;
    
    int index = x + y * stateptr->display_width;
    blend_color( &stateptr->display[index], color, factor );
}

// #####################################################################

void draw_pixelf( struct voiceprint_state *stateptr, int ix, int iy, double x, double y, guint32 color, double factor  )
{
    double dx = 1.0 - fabs(ix - x);
    double dy = 1.0 - fabs(iy - y);
    
    if( dx < 0.0 ) dx = 0.0;
    if( dy < 0.0 ) dy = 0.0;
    
    draw_pixel( stateptr, ix, iy, color, dx * dy * factor );
}

// #####################################################################

void draw_pixel_aa( struct voiceprint_state *stateptr, double x, double y, guint32 color, double factor  )
{
    // Central pixel
    draw_pixelf( stateptr, (int) x, (int) y, x, y, color, factor );

    /*draw_pixelf( stateptr, (int) x - 1, (int) y - 1, x, y, color, factor );
    draw_pixelf( stateptr, (int) x, (int) y - 1, x, y, color, factor );
    draw_pixelf( stateptr, (int) x + 1, (int) y - 1, x, y, color, factor );
    draw_pixelf( stateptr, (int) x + 1, (int) y, x, y, color, factor );
    draw_pixelf( stateptr, (int) x + 1, (int) y + 1, x, y, color, factor );
    draw_pixelf( stateptr, (int) x, (int) y + 1, x, y, color, factor );
    draw_pixelf( stateptr, (int) x - 1, (int) y + 1, x, y, color, factor );
    draw_pixelf( stateptr, (int) x - 1, (int) y, x, y, color, factor );*/

    if( x - (int) x < 0 )
    {
        // Left
        draw_pixelf( stateptr, (int) x - 1, (int) y, x, y, color, factor );
        if( y - (int) y < 0 )
        {
            // Top
            draw_pixelf( stateptr, (int) x - 1, (int) y - 1, x, y, color, factor );
            draw_pixelf( stateptr, (int) x, (int) y - 1, x, y, color, factor );
        }
        else
        {
            // Bottom
            draw_pixelf( stateptr, (int) x - 1, (int) y + 1, x, y, color, factor );
            draw_pixelf( stateptr, (int) x, (int) y + 1, x, y, color, factor );
        }
    }
    else
    {
        // Right
        draw_pixelf( stateptr, (int) x + 1, (int) y, x, y, color, factor );
        if( y - (int) y < 0 )
        {
            // Top
            draw_pixelf( stateptr, (int) x + 1, (int) y - 1, x, y, color, factor );
            draw_pixelf( stateptr, (int) x + 1, (int) y, x, y, color, factor );
        }
        else
        {
            // Bottom
            draw_pixelf( stateptr, (int) x + 1, (int) y + 1, x, y, color, factor );
            draw_pixelf( stateptr, (int) x, (int) y + 1, x, y, color, factor );
        }
    }
}

// #####################################################################

void draw_line( struct voiceprint_state *stateptr, double x0, double y0, double x1, double y1, guint32 color, double factor )
{
    int steps = abs( x1 - x0 ) + abs( y1 - y0 );
    
    if( steps == 0 ) steps = 1;
    
    double dx = x1 - x0;
    double dy = y1 - y0;
    
    for( int i = 0; i < steps; i++ )
    {
        // Make parametric
        double t = (double) i / (double) steps;
        
        double x = x0 + dx * t;
        double y = y0 + dy * t;
        
        draw_pixel_aa( stateptr, x, y, color, factor );
    }
}

// #####################################################################

void draw_border( struct voiceprint_state *stateptr,
    int border_x, int border_y, int border_width, int border_height,
    guint32 border_color )
{
    // Draw border
    for( int x = border_x; x < border_width + border_x; x++ ) stateptr->display[x + border_y * stateptr->display_width] = border_color; //blend_color( &stateptr->display[x + border_y * stateptr->display_width], border_color, 0.5 );
    for( int x = border_x; x < border_width + border_x; x++ ) stateptr->display[x + (border_y + border_height) * stateptr->display_width] = border_color; //blend_color( &stateptr->display[x + (border_y + border_height) * stateptr->display_width], border_color, 0.5 );
    for( int y = border_y; y < border_height + border_y; y++ ) stateptr->display[(border_x + border_width) + y * stateptr->display_width] = border_color; //blend_color( &stateptr->display[(border_x + border_width) + y * stateptr->display_width], border_color, 0.5 );
    for( int y = border_y; y < border_height + border_y; y++ ) stateptr->display[border_x + y * stateptr->display_width] = border_color; //blend_color( &stateptr->display[border_x + y * stateptr->display_width], border_color, 0.5 );
}

void draw_grid( struct voiceprint_state *stateptr, int grid_cols, int grid_rows,
    int grid_x, int grid_y, int grid_width, int grid_height,
    guint32 grid_color )
{
    // Vertical grid
    for( double x = grid_x; x < grid_width + grid_x; x += (double) grid_width / (double) grid_cols )
    {
        for( double y = grid_y; y < grid_height + grid_y; y += 2.0 )
        {
            int index = ((int) x) + ((int) y) * stateptr->display_width;
            //stateptr->display[index] = grid_color;
            blend_color( &stateptr->display[index], grid_color, 0.5 );
        }
    }

    // Horizontal grid
    for( double x = grid_x; x < grid_width + grid_x; x += 2.0 )
    for( double y = grid_y; y < grid_height + grid_y; y += (double) grid_height / (double) grid_rows )
    {
        int index = ((int) x) + ((int) y) * stateptr->display_width;
        //stateptr->display[index] = grid_color;
        blend_color( &stateptr->display[index], grid_color, 0.5 );
    }
}


void draw_axis( struct voiceprint_state *stateptr,
    int axis_x, int axis_y, int axis_width, int axis_height,
    guint32 axis_color )
{
    // Draw axis
    for( int x = axis_x; x < axis_width + axis_x; x++ ) stateptr->display[x + (axis_y + axis_height / 2) * stateptr->display_width] = axis_color; //blend_color( &stateptr->display[x + (axis_y + axis_height / 2) * stateptr->display_width], axis_color, 0.5 );
    for( int y = axis_y; y < axis_height + axis_y; y++ ) stateptr->display[(axis_x + axis_width / 2) + y * stateptr->display_width] = axis_color; //blend_color( &stateptr->display[(axis_x + axis_width / 2) + y * stateptr->display_width], axis_color, 0.5 );
}

// #####################################################################

// Emulates a string
/*struct spring_state
{
    double zeta;    // damping ratio
    double omega;   // undamped angular frequency of the oscillator
    double m;       // mass
    
    double x[2];    // displacement
    double z[2];
};*/

double damping( struct spring_state *spring, double val, double h )
{
    // Spring equation (UTF-8)
    //
    //   2
    //  d  x            d x     2      F(t)
    // ------ + 2 ω  ζ ----- + ω  x = ------
    //     2       0    d t     0       m
    //  d t             
    //
    // F(t) - applied force
    //
    // m - mass
    //           
    //        sqrt( k )
    // ω  =  ----------- - undamped angular frequency of the oscillator
    //  0     sqrt( m )
    //
    //            c
    // ζ = --------------- - damping ratio
    //      2 sqrt( m k )
    //
    // c - viscous damping coefficient
    //
    // k - stiffness of the system
    //
    // t - time
    //
    // Overdamped (ζ > 1): The system returns (exponentially decays)
    // to steady state without oscillating. Larger values of the
    // damping ratio ζ return to equilibrium slower.
    //
    // Critically damped (ζ = 1): The system returns to steady state
    // as quickly as possible without oscillating. This is often
    // desired for the damping of systems such as doors.
    //
    // Underdamped (ζ < 1): The system oscillates (with a slightly
    // different frequency than the undamped case) with the amplitude
    // gradually decreasing to zero. 
    //

    // Numerical solution of second order differential equation
    // using midpoint method
    //
    //               2        
    //  d x         d  x     d z             d x
    // ---- = z    ------ = -----           ----- = f(t, x, z)
    //  d t            2     d t             d t
    //              d t
    //
    //  d z     F(t)                2        d z
    // ----- = ------ - 2 ω  ζ z - ω  x     ----- = g(t, x, z)
    //  d t      m         0        0        d t
    //

    double *x = &spring->x;
    double *z = &spring->z;

#define F(_t,_x,_z) (_z)
#define G(_t,_x,_z) (val / spring->m - 2.0 * spring->zeta * spring->omega * _z - spring->omega * spring->omega * _x)

    // Euler
    //x[1] = x[0] + h * z[0];
    //z[1] = z[0] + h * (-50.0 * z[0] - 1000.0 * x[0] + 10000.0);
    //x[1] = x[0] + h * F( 0, x[0], z[0] );
    //z[1] = z[0] + h * G( 0, x[0], z[0] );

    x[1] = x[0] + h * F( 0 /* t */, x[0] + h * F( 0 /* t */, x[0], z[0] ) / 2, z[0] + h * G( 0 /* t */, x[0], z[0] ) / 2 );
    z[1] = z[0] + h * G( 0 /* t */, x[0] + h * F( 0 /* t */, x[0], z[0] ) / 2, z[0] + h * G( 0 /* t */, x[0], z[0] ) / 2 );

    // Shift
    x[0] = x[1];
    z[0] = z[1];

#undef F
#undef G

    return x[1];
}

// #####################################################################

double signal_rms( struct voiceprint_state *stateptr, gint16 *data )
{
    // Numerical integration using Simpson's rule (UTF-8)
    //
    // b
    // ⌠             b - a    ⎡            ⎛  a + b  ⎞        ⎤
    // ⌡ f(t) d t ≈ ─────── × ⎢ f(a) + 4 f ⎜ ─────── ⎟ + f(b) ⎥
    // a               6      ⎣            ⎝    2    ⎠        ⎦
    //
    // Composite Simpson's rule
    //
    // b                            n/2-1          n/2
    // ⌠             h    ⎡           ⎲             ⎲                    ⎤
    // ⌡ f(t) d t ≈ ─── × ⎢ f(t ) + 2 ⎳  f(t  ) + 4 ⎳  f(t    ) + f(t )  ⎥
    // a             3    ⎣    0     j=1    2j     j=1    2j-1       n   ⎦
    //
    //      b - a
    // h = ───────   t  = a + jh   t  = a    t  = b
    //        n       j             0         n
    //
    //         1     2
    // f(t) = ─── × A  (t) d t     a = 0     b = T
    //         T
    //


    // Period
    double T = 1.0 / (double) stateptr->fps; //((double) stateptr->rate);

    // Interval
    double a = 0;
    double b = T;
    
    // Sub intervals
    int n = stateptr->spf;
    
    // Step lenght
    double h = (b - a) / n;

#define F(x) ( pow((double) data[x] / (2 << 14), 2) )
    
    // Numerical approximation of integral f(x₀) + f(xₙ)
    double result = F(0) + F(stateptr->spf - 1);

    for( int i = 1; i < stateptr->spf / 2 - 1; i++ )
        result += 2 * F(2 * i);

    for( int i = 1; i < stateptr->spf / 2; i++ )
        result += 4 * F(2 * i - 1);

    result = (result / T) * (h / 3);

#undef F


    // Simpler way. No fun here :<
    /*double result = 0;
    for( int i = 0; i < stateptr->spf; i++ )
        result += pow((double) data[i] / (2 << 14), 2);
    result /= stateptr->spf;*/

    return result;
}

// #####################################################################
//
// SCREEN UPDATE
//

void draw_scope( struct voiceprint_state *stateptr, gint16 *data,
    int scope_x, int scope_y, int scope_width, int scope_height,
    guint32 scope_color )
{
    // Draw scope
    double x0 = scope_x;
    double y0 = scope_y + ( (double) data[0] / (2 << 15) ) * scope_height + scope_height / 2;
    for( int i = 1; i < stateptr->spf; i++ )
    {
        // Get normalized value
        double val = ( (double) data[i] / (2 << 15) );
        
        // Scale value on scope
        double x1 = scope_x + scope_width * ((double) i / (double) stateptr->spf);
        double y1 = scope_y + val * scope_height + scope_height / 2;

        draw_line( stateptr, x0, y0, x1, y1, scope_color, 1.0 );
        
        // Make end point as start point for next sample
        x0 = x1;
        y0 = y1;
        
        //int index = ((int) x1) + ((int) y1) * stateptr->display_width;
        //stateptr->display[index] = scope_color;
    }
}

// #####################################################################

void draw_voiceprint( struct voiceprint_state *stateptr, gint16 *data,
    int voice_x, int voice_y, int voice_width, int voice_height )
{
    // Copy data to fft input buffer
    for( int i = 0; i < stateptr->spf; i++ )
    {
        // Get normalized value
        double val = (double) data[i] / (2 << 15);
        
        // Store
        stateptr->fft_in[i] = val;
    }
    
    // Data process
    fftw_execute( stateptr->p );

    // Store in image
    if( stateptr->fft_image_pos + stateptr->fft_size / 2 >= stateptr->fft_image_width * stateptr->fft_size )
        stateptr->fft_image_pos = 0;
    for( int i = 0; i < stateptr->fft_size / 2; i++ )
    {
        unsigned char r;
        unsigned char g;
        unsigned char b;
        
        // Get value
        double val = cabs( stateptr->fft_out[i] );
        
        // Convert value into a color
        colormap( 
            val,
            stateptr->voice_val_range_min, stateptr->voice_val_range_max,
            &r, &g, &b
        );
        
        // Store converted value for voiceprint
        stateptr->fft_image[i + stateptr->fft_image_pos] =
            (unsigned int) b << (8 * 0) |
            (unsigned int) g << (8 * 1) |
            (unsigned int) r << (8 * 2);
        
        // Store original value for spectrum
        stateptr->fft_data[i + stateptr->fft_image_pos] = val;
    }
    
    // Shift
    stateptr->fft_image_pos +=  stateptr->fft_size / 2;
    if( stateptr->fft_image_pos >= stateptr->fft_image_width * stateptr->fft_size )
        stateptr->fft_image_pos = 0;

    // Draw FFT image on display
    for( int i = 0; i < stateptr->fft_image_width; i++ )
    for( int j = 0; j < (stateptr->fft_size / 2 > stateptr->voice_height ? stateptr->voice_height : stateptr->fft_size / 2); j++ )
    {
        int x = i + voice_x;
        int y = voice_y + voice_height - j;

        stateptr->display[x + y * stateptr->display_width] = stateptr->fft_image[i * stateptr->fft_size + j];
    }
}

// #####################################################################

void draw_spectrum( struct voiceprint_state *stateptr, double *data,
    int spectrum_x, int spectrum_y, int spectrum_width, int spectrum_height,
    guint32 spectrum_color )
{
    // Draw spectrum
    for( int i = 0; i < stateptr->fft_size / 2; i++ )
    {
        // Get normalized value
        double val = data[i] / stateptr->spectrum_val_range_max;
        
        if( val > 1.0 ) val = 1.0;
        else if( val < 0.0 ) val = 0.0;
        
        // Begin point
        double x0 = spectrum_x + spectrum_width * ((double) i / (double) stateptr->fft_size * 2);
        double y0 = spectrum_y + spectrum_height;
        
        // Scale value on scope
        double x1 = x0;
        double y1 = y0 - val * spectrum_height;

        draw_line( stateptr, x0, y0, x1, y1, spectrum_color, 1.0 );

        //int index = ((int) x1) + ((int) y1) * stateptr->display_width;
        //stateptr->display[index] = spectrum_color;
    }
}

// #####################################################################

struct thread_data
{
    struct voiceprint_state *stateptr;
    gint16 *data;
    bool fft_done;
};

void *routine_scope( struct thread_data *td )
{
    struct voiceprint_state *stateptr = td->stateptr;
    gint16 *data = td->data;

    draw_scope( stateptr, data,
        stateptr->scope_x, stateptr->scope_y, stateptr->scope_width, stateptr->scope_height,
        stateptr->scope_color );

    draw_grid( stateptr, stateptr->scope_grid_cols, stateptr->scope_grid_rows,
        stateptr->scope_grid_x, stateptr->scope_grid_y, stateptr->scope_grid_width, stateptr->scope_grid_height,
        stateptr->scope_grid_color );

    draw_border( stateptr,
        stateptr->scope_grid_x, stateptr->scope_grid_y, stateptr->scope_grid_width, stateptr->scope_grid_height,
        stateptr->scope_border_color );

    draw_axis( stateptr,
        stateptr->scope_grid_x, stateptr->scope_grid_y, stateptr->scope_grid_width, stateptr->scope_grid_height,
        stateptr->scope_axis_color );

    return NULL;
}

void *routine_voiceprint( struct thread_data *td )
{
    struct voiceprint_state *stateptr = td->stateptr;
    gint16 *data = td->data;

    draw_voiceprint( stateptr, data,
        stateptr->voice_x, stateptr->voice_y, stateptr->voice_width, stateptr->voice_height );

    // Signal that fft analyze is finished
    td->fft_done = true;

    //draw_grid( stateptr, stateptr->voice_grid_cols, stateptr->voice_grid_rows,
    //    stateptr->voice_grid_x, stateptr->voice_grid_y, stateptr->voice_grid_width, stateptr->voice_grid_height,
    //    stateptr->voice_grid_color, stateptr->voice_axis_color );

    draw_border( stateptr,
        stateptr->voice_grid_x, stateptr->voice_grid_y, stateptr->voice_grid_width, stateptr->voice_grid_height,
        stateptr->voice_border_color );

    // Draw line at current position
    const int x_cur = stateptr->voice_x + stateptr->fft_image_pos / stateptr->fft_size + 1;
    draw_line( stateptr, x_cur, stateptr->voice_y, x_cur, stateptr->voice_y + stateptr->voice_height,
        stateptr->voice_border_color, 1.0 );

    return NULL;
}

void *routine_spectrum( struct thread_data *td )
{
    struct voiceprint_state *stateptr = td->stateptr;
    gint16 *data = td->data;

    // FIXME: I don't like this
    //while( td->fft_done == true ) { }
    
    draw_spectrum( stateptr, &stateptr->fft_data[stateptr->fft_image_pos - stateptr->fft_size / 2],
        stateptr->spectrum_x, stateptr->spectrum_y, stateptr->spectrum_width, stateptr->spectrum_height,
        stateptr->spectrum_color );

    draw_grid( stateptr, stateptr->spectrum_grid_cols, stateptr->spectrum_grid_rows,
        stateptr->spectrum_grid_x, stateptr->spectrum_grid_y, stateptr->spectrum_grid_width, stateptr->spectrum_grid_height,
        stateptr->spectrum_grid_color );

    draw_border( stateptr,
        stateptr->spectrum_grid_x, stateptr->spectrum_grid_y, stateptr->spectrum_grid_width, stateptr->spectrum_grid_height,
        stateptr->spectrum_border_color );    

    return NULL;
}

void *routine_labels( struct thread_data *td )
{
    struct voiceprint_state *stateptr = td->stateptr;
    gint16 *data = td->data;

    // X-Axis labels for scope
    {
        double x_val = - 1000.0 / (2.0 * stateptr->fps);
        for( double x = stateptr->scope_grid_x; x < stateptr->scope_grid_width + stateptr->scope_grid_x + 1; x += (double) stateptr->scope_grid_width / (double) stateptr->scope_grid_cols )
        {
            char txt[20];
            snprintf( txt, sizeof(txt), "%g", x_val );
            ezd_text_centered( stateptr, txt, -1, x, stateptr->scope_grid_y + stateptr->scope_grid_height + 2, 0x0FFFFFF );
            x_val += 1000.0 / (stateptr->scope_grid_cols * stateptr->fps);
        }
    }
    
    // Y-Axis labels for scope
    {
        double y_val = 1;
        for( double y = stateptr->scope_grid_y; y < stateptr->scope_grid_height + stateptr->scope_grid_y + 1; y += (double) stateptr->scope_grid_height / (double) stateptr->scope_grid_rows )
        {
            char txt[20];
            snprintf( txt, sizeof(txt), "%.1f", y_val );
            ezd_text_right( stateptr, txt, -1, stateptr->scope_grid_x - 2, y - 4, 0x0FFFFFF );
            y_val -= 2 / (double) stateptr->scope_grid_rows;
        }
    }
    
    // Y-Axis labels for voiceprint
    {
        double freq = 0;
        int i = -2;
        while( 1 )
        {
            double y = stateptr->voice_grid_height + stateptr->voice_grid_y;
            y -= freq * (double) stateptr->spf / (double) stateptr->rate;
            
            // Stop if we are outside the plot
            if( y < stateptr->voice_grid_y )
                break;
            
            char txt[20];
            
            // Label
            snprintf( txt, sizeof(txt), "%0.0f", freq );
            ezd_text_right( stateptr, txt, -1, stateptr->voice_grid_x - 2, y - 4, 0x0FFFFFF );
            
            // Tic line
            draw_line( stateptr, stateptr->voice_grid_x, y, stateptr->voice_grid_x + 5, y,
                stateptr->voice_border_color, 0.75 );
            
            // Mark octave
            if( i % 12 == 0 )
                draw_line( stateptr, stateptr->voice_grid_x, y, stateptr->voice_grid_x + 10, y,
                    stateptr->voice_border_color, 1.0 );
            
            // Next frequency
            //freq += 500;
            freq = 440.0 * pow( 2.0, (double) i / 12.0 );
            i++;

            if( freq > stateptr->rate )
                break;
        }
    }

    // X-Axis labels for voiceprint
    {
        double x_val = 0;
        double time = 0; // Seconds
        while( 1 )
        {
            double x = stateptr->voice_grid_x;
            x += time * (double) stateptr->fps;
            
            // Stop if we are outside the plot
            if( x > stateptr->voice_grid_x + stateptr->voice_grid_width )
                break;
            
            char txt[20];
            
            // Label
            snprintf( txt, sizeof(txt), "%0.0f", time );
            ezd_text_centered( stateptr, txt, -1, x, stateptr->voice_grid_y + stateptr->voice_grid_height + 2, 0x0FFFFFF );
            
            // Tic line
            draw_line( stateptr, x, stateptr->voice_grid_y + stateptr->voice_grid_height, x, stateptr->voice_grid_y + stateptr->voice_grid_height - 5,
                stateptr->voice_border_color, 0.75 );

            // Next time tic
            time += 1.0;
        }
    }

    // X-Axis labels for spectrum
    {
        double freq = 0;
        int i = 0;
        while( 1 )
        {
            double x = stateptr->spectrum_grid_x;
            x += (double) stateptr->spectrum_grid_width * freq * 2.0 / (double) stateptr->rate;
            
            // Stop if we are outside the plot
            if( x > stateptr->spectrum_grid_x + stateptr->spectrum_grid_width )
                break;
            
            char txt[20];
            
            // Label
            snprintf( txt, sizeof(txt), "%0.0f", freq / 1000.0 );
            ezd_text_centered( stateptr, txt, -1, x, stateptr->spectrum_grid_y + stateptr->spectrum_height + 3, 0x0FFFFFF );
            
            // Tic line
            //draw_line( stateptr, x, stateptr->spectrum_grid_y + stateptr->spectrum_height, x, stateptr->spectrum_grid_y + stateptr->spectrum_height + 1,
            //    stateptr->spectrum_border_color, 0.75 );

            // Vertical grid
            for( double y = stateptr->spectrum_grid_y; y < stateptr->spectrum_grid_y + stateptr->spectrum_height; y += 2.0 )
            {
                int index = ((int) x) + ((int) y) * stateptr->display_width;
                //stateptr->display[index] = grid_color;
                blend_color( &stateptr->display[index], stateptr->spectrum_border_color, 0.5 );
            }

            // Next frequency
            freq += 1000;
            //freq = 440.0 * pow( 2.0, (double) i / 12.0 );
            i++;

            if( freq > stateptr->rate )
                break;
        }
    }

    // Y-Axis labels for spectrum
    {
        double y_val = 0.0;
        while( 1 )
        {
            // Calculate position on display
            double y = stateptr->spectrum_grid_y + stateptr->spectrum_grid_height;
            y -= y_val * stateptr->spectrum_grid_height / stateptr->spectrum_val_range_max;
            
            // Exit if outside the plot
            if( y < stateptr->spectrum_grid_y )
                break;
            
            // Print
            char txt[20];
            snprintf( txt, sizeof(txt), "%.1f", y_val );
            ezd_text_right( stateptr, txt, -1, stateptr->spectrum_grid_x - 1, y - 4, 0x0FFFFFF );

            // Horizontal grid
            for( double x = stateptr->spectrum_grid_x; x < stateptr->spectrum_grid_width + stateptr->spectrum_grid_x; x += 2.0 )
            {
                int index = ((int) x) + ((int) y) * stateptr->display_width;
                //stateptr->display[index] = grid_color;
                blend_color( &stateptr->display[index], stateptr->spectrum_grid_color, 0.5 );
            }

            // Next label
            y_val += 2.0;
        }
    }

    ezd_text( stateptr, "Realtime oscilloscope and voiceprint visualization by Valentin But (valentin.but@eesti.ee). Enjoy.", -1, 50, 1, 0x0FFFFFF );

    {
        char txt[2000];
        
        // General info
        snprintf( txt, sizeof(txt), "Sample Rate: %i Hz, Samples per Frame: %i, Frames per Second: %i/%i, Dimensions: %ix%i",
            stateptr->rate, stateptr->spf, stateptr->fps_num, stateptr->fps_denom, stateptr->display_width, stateptr->display_height );
        ezd_text( stateptr, txt, -1, 50, stateptr->voice_grid_y - 9, 0x0A0A0A0 );
        
        // Date time
        time_t t = time( NULL );
        struct tm *tmp = localtime(&t);
        strftime(txt, sizeof(txt), "Date: %d.%m.%Y  Time: %H:%M:%S", tmp );
        ezd_text( stateptr, txt, -1, 900, stateptr->voice_grid_y - 9, 0x000A0A0 );
        
        // Unit labels
        ezd_text_right( stateptr, "f, kHz", -1, stateptr->spectrum_grid_x + stateptr->spectrum_grid_width, stateptr->spectrum_grid_y + stateptr->spectrum_grid_height + 3 + 10 - 2, 0x0A0A000 );
        ezd_text_right( stateptr, "t, ms", -1, stateptr->scope_grid_x + stateptr->scope_grid_width, stateptr->scope_grid_y + stateptr->scope_grid_height + 3 + 10 - 2, 0x0A0A000 );
    }
    
    return NULL;
}

void *routine_soundlevel( struct thread_data *td )
{
    struct voiceprint_state *stateptr = td->stateptr;
    gint16 *data = td->data;

    //while( td->fft_done == false ) { }

    int level_x = 1209;
    int level_y = 27;
    int level_width = 310;
    int level_height = 172;

    // Measure root mean square
    double val = signal_rms( stateptr, data ) / 0.2;

    // Convert to db
    //                   A₁     A₀ - reference amplitude
    // L   = 20 log   ( ---- )  A₁ - measured amplitude
    //  dB         10    A₀
    //
    double reference = 1.0;

    double displacement = damping( &stateptr->spring, val, 1.0 / stateptr->fps );

    double angle_min = 57.0 * M_PI / 180.0;
    double angle_mid = 104.3 * M_PI / 180.0;
    double angle_max = 123.5 * M_PI / 180.0;
    double angle_current = (angle_mid - angle_min) * displacement + angle_min;
    double line_lenght = level_height * 1.3;
    double arrow_offset_y = level_height / 2;

    limit_vald( &angle_current, angle_min, angle_max );

#define ANGLE_LINE(x,y,r0,r1,phi,color) draw_line( stateptr, x + r0 * sin( phi - M_PI / 2.0 ), y - r0 * cos( phi - M_PI / 2.0 ), x + r1 * sin( phi - M_PI / 2.0 ), y - r1 * cos( phi - M_PI / 2.0 ), color, 1.0 );
#define ANGLE_TEXT(text,x,y,r,phi,color) ezd_text_centered( stateptr, text, -1, x + r * sin( phi - M_PI / 2.0 ), -5 + y - r * cos( phi - M_PI / 2.0 ), color );

    //draw_line( stateptr, 3.0, 1.0, 3.0, 100.0, 0x000FF00, 1.0 );
    //draw_line( stateptr, 5.0, 1.0, 5.0, val, 0x0FF0000, 1.0 );

    // Min Mid Max
    ANGLE_LINE( level_x + level_width / 2, level_y + level_height + arrow_offset_y, line_lenght * 0.92, line_lenght * 0.95, angle_min, 0x0FFFFFF );
    ANGLE_LINE( level_x + level_width / 2, level_y + level_height + arrow_offset_y, line_lenght * 0.92, line_lenght * 0.95, angle_min + (angle_mid - angle_min) / 2, 0x0FFFFFF );
    ANGLE_LINE( level_x + level_width / 2, level_y + level_height + arrow_offset_y, line_lenght * 0.92, line_lenght * 0.95, angle_mid, 0x0FF0000 );
    //ANGLE_LINE( level_x + level_width / 2, level_y + level_height + arrow_offset_y, line_lenght * 0.90, line_lenght * 0.92, angle_max, 0x0FF0000 );
    ANGLE_TEXT( "0", level_x + level_width / 2, level_y + level_height + arrow_offset_y, line_lenght * 0.86, angle_min, 0x0FFFFFF );
    ANGLE_TEXT( "50", level_x + level_width / 2, level_y + level_height + arrow_offset_y, line_lenght * 0.86, angle_min + (angle_mid - angle_min) / 2, 0x0FFFFFF );
    ANGLE_TEXT( "100", level_x + level_width / 2, level_y + level_height + arrow_offset_y, line_lenght * 0.86, angle_mid, 0x0FFFFFF );
    //ANGLE_TEXT( "+", level_x + level_width / 2, level_y + level_height + arrow_offset_y, line_lenght * 0.85, angle_max, 0x0FFFFFF );

    // Decibels
    double Ldb[] = { -20, -10, -7, -5, -3, -1, 0, 1, 2, 3 };
    for( int i = 0; i < sizeof(Ldb) / sizeof(double); i++ )
    {
        double quantity = pow(10, Ldb[i] / 20) * reference;
        double db_angle = angle_min + quantity * (angle_mid - angle_min);
        
        int color = 0x0FFFFFF;
        if( Ldb[i] >= 0.0 )
            color = 0x0FF0000;
        
        ANGLE_LINE( level_x + level_width / 2, level_y + level_height + arrow_offset_y, line_lenght * 0.95, line_lenght, db_angle, color );
        
        char txt[50];
        snprintf( txt, sizeof(txt), "%g", Ldb[i] );
        ANGLE_TEXT( txt, level_x + level_width / 2, level_y + level_height + arrow_offset_y, line_lenght * 1.05, db_angle, color ); 
    }

    // Arcs
    for( double angle = angle_min; angle < 0.98 * angle_mid; angle += M_PI / (line_lenght * 4.0) )
    {
        double x = level_x + level_width / 2 + 0.95 * line_lenght * sin( angle - M_PI / 2.0 );
        double y = level_y + level_height + arrow_offset_y - 0.95 * line_lenght * cos( angle - M_PI / 2.0 );

        draw_pixel_aa( stateptr, x, y, 0x0FFFFFF, 1.0 );
    }
    for( double angle = angle_mid; angle < angle_max; angle += M_PI / (line_lenght * 4.0) )
    {
        double x = level_x + level_width / 2 + 0.95 * line_lenght * sin( angle - M_PI / 2.0 );
        double y = level_y + level_height + arrow_offset_y - 0.95 * line_lenght * cos( angle - M_PI / 2.0 );

        draw_pixel_aa( stateptr, x, y, 0x0FF0000, 1.0 );
    }

    // Meter units
    ezd_text_centered( stateptr, "dB", -1, level_x + level_width / 2, level_y + level_height / 2, 0x0FFFFFF );

    // Line displaying current value
    ANGLE_LINE( level_x + level_width / 2, level_y + level_height + arrow_offset_y, abs(arrow_offset_y) / cos(M_PI / 2 - angle_current), line_lenght, angle_current, 0x0FFFFFF );

#undef ANGLE_LINE
#undef ANGLE_TEXT

    char txt[50];
    snprintf( txt, sizeof(txt), "RMS = %.03f (0.3 = 100%)", val, displacement );
    ezd_text( stateptr, txt, -1, level_x, level_y - 10, 0x0FFFFFF );
    
    draw_border( stateptr, level_x, level_y, level_width, level_height, 0x0FFFFFF );
    
    return NULL;
}

guint32 *voiceprint_update( struct voiceprint_state *stateptr, gint16 *data )
{
    // display color
    // 0x0 00 00 FF 00
    //     xx RR GG BB

    // Reset display
    memset( stateptr->display, 0, (stateptr->display_width + 1) * (stateptr->display_height + 1) * sizeof(guint32) );

    struct thread_data td =
    {
        .stateptr = stateptr,
        .data = data,
        
        // Spectrum cannot be drawn before fft calculations are done
        .fft_done = false
    };

    //
    // SCOPE
    //

    pthread_t thread_scope_id;
    pthread_create( &thread_scope_id, NULL, routine_scope, &td );

    //
    // VOICEPRINT
    //

    pthread_t thread_voiceprint_id;
    pthread_create( &thread_voiceprint_id, NULL, routine_voiceprint, &td );

    //
    // SPECTRUM
    //

    pthread_t thread_spectrum_id;
    pthread_create( &thread_spectrum_id, NULL, routine_spectrum, &td );

    //
    // SOUNDLEVEL
    //
    
    pthread_t thread_soundlevel_id;
    pthread_create( &thread_soundlevel_id, NULL, routine_soundlevel, &td );

    //
    // TEXT
    //

    pthread_t thread_labels_id;
    pthread_create( &thread_labels_id, NULL, routine_labels, &td );

    //  Wait for the threads to terminate
    pthread_join( thread_scope_id, NULL );
    pthread_join( thread_voiceprint_id, NULL );
    pthread_join( thread_spectrum_id, NULL );
    pthread_join( thread_labels_id, NULL );
    pthread_join( thread_soundlevel_id, NULL );

    return stateptr->display;
}

// #####################################################################
