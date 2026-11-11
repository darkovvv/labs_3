#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
    STATUS_OK = 0,
    STATUS_INVALID_ARGUMENT,
    STATUS_FILE_ERROR,
    STATUS_MEMORY_ERROR
} StatusCode;

typedef struct HuffNode {
    unsigned char bajt_znach;
    unsigned long long chastota;
    int est_list;
    struct HuffNode *lev;
    struct HuffNode *prav;
} HuffNode;

typedef struct {
    HuffNode **uzly;
    size_t razmer;
    size_t emkost;
} MinHeap;

/* создание ноды дерева хаффмана */
static HuffNode *create_node(unsigned char bajt_znach,
                                unsigned long long chastota,
                                int est_list,
                                HuffNode *lev,
                                HuffNode *prav)
{
    HuffNode *uzel = (HuffNode *)malloc(sizeof(HuffNode));
    if (uzel == NULL) {
        return NULL;
    }
    uzel->bajt_znach = bajt_znach;
    uzel->chastota = chastota;
    uzel->est_list = est_list;
    uzel->lev = lev;
    uzel->prav = prav;
    return uzel;
}

static void free_tree(HuffNode *koren)
{
    if (koren == NULL) {
        return;
    }
    free_tree(koren->lev);
    free_tree(koren->prav);
    free(koren);
}

static StatusCode heap_init(MinHeap *kucha, size_t emkost)
{
    if (kucha == NULL || emkost == 0U) {
        return STATUS_INVALID_ARGUMENT;
    }
    kucha->uzly = (HuffNode **)malloc(emkost * sizeof(HuffNode *));
    if (kucha->uzly == NULL) {
        return STATUS_MEMORY_ERROR;
    }
    kucha->razmer = 0U;
    kucha->emkost = emkost;
    return STATUS_OK;
}

/* пуш в кучу с просеиванием вверх */
static StatusCode heap_push(MinHeap *kucha, HuffNode *uzel)
{
    size_t ind;

    if (kucha == NULL || uzel == NULL || kucha->razmer >= kucha->emkost) {
        return STATUS_INVALID_ARGUMENT;
    }

    ind = kucha->razmer++;
    kucha->uzly[ind] = uzel;

    while (ind > 0U) {
        size_t roditel = (ind - 1U) / 2U;
        if (kucha->uzly[roditel]->chastota <= kucha->uzly[ind]->chastota) {
            break;
        }
        {
            HuffNode *vrem = kucha->uzly[roditel];
            kucha->uzly[roditel] = kucha->uzly[ind];
            kucha->uzly[ind] = vrem;
        }
        ind = roditel;
    }
    return STATUS_OK;
}

/* извлечение минимума с просеиванием вниз */
static HuffNode *heap_pop(MinHeap *kucha)
{
    HuffNode *verh;
    size_t ind = 0U;

    if (kucha == NULL || kucha->razmer == 0U) {
        return NULL;
    }

    verh = kucha->uzly[0];
    kucha->uzly[0] = kucha->uzly[--kucha->razmer];

    for (;;) {
        size_t lev = 2U * ind + 1U;
        size_t prav = 2U * ind + 2U;
        size_t min_ind = ind;

        if (lev < kucha->razmer &&
            kucha->uzly[lev]->chastota < kucha->uzly[min_ind]->chastota) {
            min_ind = lev;
        }
        if (prav < kucha->razmer &&
            kucha->uzly[prav]->chastota < kucha->uzly[min_ind]->chastota) {
            min_ind = prav;
        }
        if (min_ind == ind) {
            break;
        }
        {
            HuffNode *vrem = kucha->uzly[ind];
            kucha->uzly[ind] = kucha->uzly[min_ind];
            kucha->uzly[min_ind] = vrem;
        }
        ind = min_ind;
    }

    return verh;
}

/* обход дерева и сохранение кодов */
static void build_codes(const HuffNode *uzel,
                        char tek_kod[257],
                        size_t glubina,
                        char kody[256][257])
{
    if (uzel == NULL) {
        return;
    }
    if (uzel->est_list) {
        if (glubina == 0U) {
            kody[uzel->bajt_znach][0] = '0';
            kody[uzel->bajt_znach][1] = '\0';
        } else {
            memcpy(kody[uzel->bajt_znach], tek_kod, glubina);
            kody[uzel->bajt_znach][glubina] = '\0';
        }
        return;
    }

    tek_kod[glubina] = '0';
    build_codes(uzel->lev, tek_kod, glubina + 1U, kody);

    tek_kod[glubina] = '1';
    build_codes(uzel->prav, tek_kod, glubina + 1U, kody);
}

static StatusCode analyze_file_huffman(const char *put_f,
                                       unsigned long long chastoty[256],
                                       char kody[256][257],
                                       unsigned long long *obsh_kolvo_bytes_out,
                                       unsigned long long *kodir_bity_out)
{
    FILE *file = NULL;
    int ch;
    size_t kolvo_unik = 0U;
    size_t i;
    MinHeap kucha;
    HuffNode *koren = NULL;
    char vrem_kod[257];
    StatusCode st_kod;

    if (put_f == NULL || chastoty == NULL || kody == NULL ||
        obsh_kolvo_bytes_out == NULL || kodir_bity_out == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }

    for (i = 0; i < 256U; ++i) {
        chastoty[i] = 0ULL;
        kody[i][0] = '\0';
    }
    *obsh_kolvo_bytes_out = 0ULL;
    *kodir_bity_out = 0ULL;

    file = fopen(put_f, "rb");
    if (file == NULL) {
        return STATUS_FILE_ERROR;
    }

    while ((ch = fgetc(file)) != EOF) {
        chastoty[(unsigned char)ch]++;
        (*obsh_kolvo_bytes_out)++;
    }
    if (ferror(file)) {
        fclose(file);
        return STATUS_FILE_ERROR;
    }
    fclose(file);

    if (*obsh_kolvo_bytes_out == 0ULL) {
        return STATUS_OK;
    }

    for (i = 0; i < 256U; ++i) {
        if (chastoty[i] > 0ULL) {
            kolvo_unik++;
        }
    }

    st_kod = heap_init(&kucha, kolvo_unik);
    if (st_kod != STATUS_OK) {
        return st_kod;
    }

    for (i = 0; i < 256U; ++i) {
        if (chastoty[i] > 0ULL) {
            HuffNode *list_uz = create_node((unsigned char)i, chastoty[i], 1, NULL, NULL);
            if (list_uz == NULL) {
                while (kucha.razmer > 0U) {
                    free_tree(heap_pop(&kucha));
                }
                free(kucha.uzly);
                return STATUS_MEMORY_ERROR;
            }
            heap_push(&kucha, list_uz);
        }
    }

    /* слияние двух минимальных нод */
    while (kucha.razmer > 1U) {
        HuffNode *perv = heap_pop(&kucha);
        HuffNode *vtor = heap_pop(&kucha);
        HuffNode *roditel = create_node(0U,
                                          perv->chastota + vtor->chastota,
                                          0,
                                          perv,
                                          vtor);
        if (roditel == NULL) {
            free_tree(perv);
            free_tree(vtor);
            while (kucha.razmer > 0U) {
                free_tree(heap_pop(&kucha));
            }
            free(kucha.uzly);
            return STATUS_MEMORY_ERROR;
        }
        heap_push(&kucha, roditel);
    }

    koren = heap_pop(&kucha);
    free(kucha.uzly);

    build_codes(koren, vrem_kod, 0U, kody);
    free_tree(koren);

    /* подсчет суммарного веса в битах */
    for (i = 0; i < 256U; ++i) {
        if (chastoty[i] > 0ULL) {
            *kodir_bity_out += chastoty[i] * (unsigned long long)strlen(kody[i]);
        }
    }

    return STATUS_OK;
}

int main(int argc, char **argv)
{
    unsigned long long chastoty[256];
    char kody[256][257];
    unsigned long long ish_bajty = 0ULL;
    unsigned long long kodir_bity = 0ULL;
    unsigned long long kodir_bajty = 0ULL;
    double ekonomiya = 0.0;
    StatusCode st_kod;
    int i;

    if (argc != 2) {
        fprintf(stderr, "Использование: %s <путь_к_файлу>\n", argv[0]);
        return 1;
    }

    st_kod = analyze_file_huffman(argv[1], chastoty, kody, &ish_bajty, &kodir_bity);
    if (st_kod != STATUS_OK) {
        fprintf(stderr, "Ошибка при обработке файла (код %d).\n", (int)st_kod);
        return 1;
    }

    printf("Таблица кодов Хаффмана (байт | частота | код):\n");
    if (ish_bajty > 0ULL) {
        for (i = 0; i < 256; ++i) {
            if (chastoty[i] > 0ULL) {
                printf("%3d | %10llu | %s\n", i, chastoty[i], kody[i]);
            }
        }
        kodir_bajty = (kodir_bity + 7ULL) / 8ULL;
        ekonomiya = 100.0 * (1.0 - (double)kodir_bajty / (double)ish_bajty);
    }

    printf("\nИсходный размер (байт):        %llu\n", ish_bajty);
    printf("Закодированный размер (бит):   %llu\n", kodir_bity);
    printf("Закодированный размер (байт):  %llu\n", kodir_bajty);
    printf("Экономия:                      %.2f%%\n", ekonomiya);

    return 0;
}
