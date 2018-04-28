#include <SDL2/SDL.h>
#include <assert.h>

enum {
        CHR_WIDTH = 128,
        CHR_SIZE = 128 * 128 * 2,
};

int main(int argc, char **argv)
{
        SDL_Init(SDL_INIT_VIDEO);
        char *inpath = argc > 1 ? argv[1] : "chr.bmp";
        char *outpath = argc > 2 ? argv[2] : "chr.bin";
        const SDL_Surface *s = SDL_LoadBMP(inpath);
        FILE *out = fopen(outpath, "w");

        char *bmp = s->pixels;
        Uint64 *buf = calloc(1, CHR_SIZE / 8);
        assert(s->w == CHR_WIDTH && s->h == CHR_WIDTH);
        for (int t=0; t<256; ++t) {
                char *tile = &bmp[(t%16)*8 + (t/16)*CHR_WIDTH*8];
                for (int p=0; p<64; ++p) {
                        long b = tile[(7-p%8) + (p/8)*CHR_WIDTH];
                        buf[t*2+0] |= (b >> 0 & 1) << p;
                        buf[t*2+1] |= (b >> 1 & 1) << p;
                }
        }

        fwrite(buf, 1, CHR_SIZE / 8, out);

        fclose(out);
        SDL_Quit();
}
