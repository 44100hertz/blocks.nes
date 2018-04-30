#include <SDL2/SDL.h>
#include <assert.h>

int main(int argc, char **argv)
{
        SDL_Init(SDL_INIT_VIDEO);
        char *inpath = argc > 1 ? argv[1] : "chr.bmp";
        char *outpath = argc > 2 ? argv[2] : "chr.bin";
        const SDL_Surface *s = SDL_LoadBMP(inpath);
        FILE *out = fopen(outpath, "w");

        char *bmp = s->pixels;
        const size_t bufsize = s->w * s->h * 2 / 8;
        Uint64 *buf = calloc(1, bufsize);
        Uint64 tiles_x = s->w / 8;
        for (Uint64 t=0; t<s->w*s->h/64; ++t) {
                char *tile = &bmp[(t%tiles_x)*8 + (t/tiles_x)*s->w*8];
                for (Uint64 p=0; p<64; ++p) {
                        long b = tile[(7-p%8) + (p/8)*s->w];
                        buf[t*2+0] |= (b >> 0 & 1) << p;
                        buf[t*2+1] |= (b >> 1 & 1) << p;
                }
        }
        fwrite(buf, 1, bufsize, out);

        fclose(out);
        SDL_Quit();
}
