#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

typedef enum {
    STATUS_OK = 0,
    STATUS_INVALID_ARGUMENT,
    STATUS_MEMORY_ERROR
} StatusCode;

/* битовое сложение без плюса */
static unsigned int bit_add(unsigned int a, unsigned int b)
{
    while (b != 0U) {
        unsigned int perenos = (a & b) << 1U;
        a = a ^ b;
        b = perenos;
    }
    return a;
}

/* вычитание через доп код */
static unsigned int bit_sub(unsigned int a, unsigned int b)
{
    return bit_add(a, bit_add(~b, 1U));
}

/* перевод числа через маску и сдвиг */
static StatusCode convert_to_power_of_two_base(int chislo, int r, char *buf_out, size_t razmer_buf)
{
    const char alfavit[] = "0123456789ABCDEFGHIJKLMNOPQRSTUV";
    char vrem_buf[64];
    unsigned int znach;
    unsigned int maska;
    unsigned int sdvig;
    unsigned int dlina_vrem = 0U;
    unsigned int ind_vyh = 0U;
    int otric = 0;

    if (buf_out == NULL || razmer_buf < 2U || r < 1 || r > 5) {
        return STATUS_INVALID_ARGUMENT;
    }

    if (chislo == 0) {
        buf_out[0] = '0';
        buf_out[1] = '\0';
        return STATUS_OK;
    }

    if (chislo < 0) {
        otric = 1;
        znach = bit_add(~(unsigned int)chislo, 1U);
    } else {
        znach = (unsigned int)chislo;
    }

    sdvig = (unsigned int)r;
    maska = bit_sub(1U << sdvig, 1U);

    /* нарезка битов маской */
    while (znach != 0U) {
        unsigned int cifra = znach & maska;
        vrem_buf[dlina_vrem] = alfavit[cifra];
        dlina_vrem = bit_add(dlina_vrem, 1U);
        znach >>= sdvig;
    }

    if (otric) {
        vrem_buf[dlina_vrem] = '-';
        dlina_vrem = bit_add(dlina_vrem, 1U);
    }

    if ((size_t)bit_add(dlina_vrem, 1U) > razmer_buf) {
        return STATUS_MEMORY_ERROR;
    }

    /* разворот буфера в результат */
    while (dlina_vrem != 0U) {
        dlina_vrem = bit_sub(dlina_vrem, 1U);
        buf_out[ind_vyh] = vrem_buf[dlina_vrem];
        ind_vyh = bit_add(ind_vyh, 1U);
    }
    buf_out[ind_vyh] = '\0';

    return STATUS_OK;
}

int main(void)
{
    const int test_chisla[] = {0, 42, -42, 255, -1024, 1234567, INT_MAX, INT_MIN};
    size_t kolvo_testov = sizeof(test_chisla) / sizeof(test_chisla[0]);
    size_t i;
    int r;

    for (i = 0; i < kolvo_testov; ++i) {
        int tek_chislo = test_chisla[i];
        printf("Число: %d\n", tek_chislo);
        for (r = 1; r <= 5; ++r) {
            char rez_str[64];
            StatusCode st_kod = convert_to_power_of_two_base(tek_chislo, r, rez_str, sizeof(rez_str));
            if (st_kod == STATUS_OK) {
                printf("  r = %d (база %2d): %s\n", r, 1 << r, rez_str);
            } else {
                printf("  r = %d: ошибка %d\n", r, (int)st_kod);
            }
        }
        putchar('\n');
    }

    return 0;
}
