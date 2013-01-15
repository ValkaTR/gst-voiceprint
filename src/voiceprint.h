
#ifndef _VOICEPRINT_H
#define _VOICEPRINT_H

#include <glib.h>

#include <math.h>
#include <complex.h>
#include <fftw3.h>

// Emulates a string
struct spring_state
{
    double zeta;    // damping ratio
    double omega;   // undamped angular frequency of the oscillator
    double m;       // mass
    
    double x[2];    // displacement
    double z[2];
};

struct voiceprint_state
{
	// Display info and memory
	guint32 display_width;
	guint32 display_height;
    guint32 *display;

	// Scope dimensions and colors
    int scope_x;
    int scope_y;
    int scope_width;
    int scope_height;    
    guint32 scope_color;

	// Scope grid dimensions and colors
	guint32 scope_grid_color;
	guint32 scope_border_color;
	guint32 scope_axis_color;
	int scope_grid_cols;
	int scope_grid_rows;
	int scope_grid_x;
	int scope_grid_y;
	int scope_grid_width;
	int scope_grid_height;

	// Voiceprint settings
    int voice_x;
    int voice_y;
    int voice_width;
    int voice_height;

	// Voiceprint grid settings
	double voice_val_range_min;
	double voice_val_range_max;
	guint32 voice_grid_color;
	guint32 voice_border_color;
	guint32 voice_axis_color;
	int voice_grid_cols;
	int voice_grid_rows;
	int voice_grid_x;
	int voice_grid_y;
	int voice_grid_width;
	int voice_grid_height;

	// Spectrum settings
    guint32 spectrum_color;
    int spectrum_x;
    int spectrum_y;
    int spectrum_width;
    int spectrum_height;

	// Spectrum grid
	double spectrum_val_range_min;
	double spectrum_val_range_max;
	guint32 spectrum_grid_color;
	guint32 spectrum_border_color;
	guint32 spectrum_axis_color;
	int spectrum_grid_cols;
	int spectrum_grid_rows;
	int spectrum_grid_x;
	int spectrum_grid_y;
	int spectrum_grid_width;
	int spectrum_grid_height;	

	// Sound level data
	struct spring_state spring;

	// Data info
	int fps_denom;
	int fps_num;
	double fps; // = fps_num / fps_denom
	int rate;
	int spf; // Samples per Frame

	// FFT settings
	int fft_size;
    
    // FFT buffers
    size_t fft_image_size;
    size_t fft_image_width;
    size_t fft_data_size;
    guint32 *fft_image;
    double *fft_data;
    size_t fft_image_pos;

    // FFT handlers
    double *fft_in;
    fftw_complex *fft_out;
    fftw_plan p;
};

struct voiceprint_state *voiceprint_init( guint32 resx, guint32 resy, int fps_denom, int fps_num, int rate, int spf );
guint32 *voiceprint_update( struct voiceprint_state *stateptr, gint16 *data );
void voiceprint_close( struct voiceprint_state *stateptr );

#endif
