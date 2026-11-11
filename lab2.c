#include <ctype.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

typedef enum {
    STATUS_OK = 0,
    STATUS_INVALID_ARGUMENT,
    STATUS_MEMORY_ERROR
} StatusCode;

typedef struct {
    int id;
    int nach_min;
    int konec_min;
} TalkReq;

/* компаратор по времени конца */
static int compare_talks(const void *lev_ukaz, const void *prav_ukaz)
{
    const TalkReq *lev = (const TalkReq *)lev_ukaz;
    const TalkReq *prav = (const TalkReq *)prav_ukaz;

    if (lev->konec_min != prav->konec_min) {
        return (lev->konec_min > prav->konec_min) -
               (lev->konec_min < prav->konec_min);
    }
    if (lev->nach_min != prav->nach_min) {
        return (lev->nach_min > prav->nach_min) -
               (lev->nach_min < prav->nach_min);
    }
    return (lev->id > prav->id) - (lev->id < prav->id);
}

/* парсинг времени hh:mm */
static int parse_time_token(const char *tok, int *minuty_out)
{
    int chasy;
    int minuty;

    if (tok == NULL || minuty_out == NULL) {
        return 0;
    }
    if (!isdigit((unsigned char)tok[0]) ||
        !isdigit((unsigned char)tok[1]) ||
        tok[2] != ':' ||
        !isdigit((unsigned char)tok[3]) ||
        !isdigit((unsigned char)tok[4]) ||
        tok[5] != '\0') {
        return 0;
    }

    chasy = (tok[0] - '0') * 10 + (tok[1] - '0');
    minuty = (tok[3] - '0') * 10 + (tok[4] - '0');
    if (chasy < 0 || chasy > 23 || minuty < 0 || minuty > 59) {
        return 0;
    }

    *minuty_out = chasy * 60 + minuty;
    return 1;
}

/* жадное распределение докладов по залам */
static StatusCode max_scheduled_talks(const TalkReq *zayavki,
                                      size_t kolvo,
                                      int kolvo_zalov,
                                      int *max_dokl_out)
{
    TalkReq *otsort = NULL;
    int *konec_zala = NULL;
    size_t i;
    size_t j;
    int kolvo_vybr = 0;

    if (zayavki == NULL || max_dokl_out == NULL || kolvo == 0U || kolvo_zalov <= 0) {
        return STATUS_INVALID_ARGUMENT;
    }

    /* валидация айдишников и интервалов */
    for (i = 0; i < kolvo; ++i) {
        if (zayavki[i].id <= 0 ||
            zayavki[i].nach_min < 0 ||
            zayavki[i].konec_min > 24 * 60 ||
            zayavki[i].nach_min >= zayavki[i].konec_min) {
            return STATUS_INVALID_ARGUMENT;
        }
        for (j = i + 1U; j < kolvo; ++j) {
            if (zayavki[i].id == zayavki[j].id) {
                return STATUS_INVALID_ARGUMENT;
            }
        }
    }

    otsort = (TalkReq *)malloc(kolvo * sizeof(TalkReq));
    if (otsort == NULL) {
        return STATUS_MEMORY_ERROR;
    }
    konec_zala = (int *)malloc((size_t)kolvo_zalov * sizeof(int));
    if (konec_zala == NULL) {
        free(otsort);
        return STATUS_MEMORY_ERROR;
    }

    for (i = 0; i < kolvo; ++i) {
        otsort[i] = zayavki[i];
    }
    for (i = 0; i < (size_t)kolvo_zalov; ++i) {
        konec_zala[i] = 0;
    }

    qsort(otsort, kolvo, sizeof(TalkReq), compare_talks);

    /* выбор зала что освободился позже всех */
    for (i = 0; i < kolvo; ++i) {
        int luchsh_zal = -1;
        int luchsh_konec = -1;
        int r;

        for (r = 0; r < kolvo_zalov; ++r) {
            if (konec_zala[r] <= otsort[i].nach_min &&
                konec_zala[r] > luchsh_konec) {
                luchsh_konec = konec_zala[r];
                luchsh_zal = r;
            }
        }

        if (luchsh_zal != -1) {
            konec_zala[luchsh_zal] = otsort[i].konec_min;
            ++kolvo_vybr;
        }
    }

    free(konec_zala);
    free(otsort);
    *max_dokl_out = kolvo_vybr;
    return STATUS_OK;
}

int main(void)
{
    int n;
    int k;
    int i;
    int otvet = 0;
    TalkReq *zayavki = NULL;
    StatusCode st_kod;

    if (scanf("%d %d", &n, &k) != 2 || n <= 0 || k <= 0) {
        fprintf(stderr, "Некорректные входные данные n и k.\n");
        return 1;
    }

    zayavki = (TalkReq *)malloc((size_t)n * sizeof(TalkReq));
    if (zayavki == NULL) {
        fprintf(stderr, "Ошибка выделения памяти.\n");
        return 1;
    }

    for (i = 0; i < n; ++i) {
        char nach_str[32];
        char konec_str[32];

        if (scanf("%d %31s %31s", &zayavki[i].id, nach_str, konec_str) != 3 ||
            !parse_time_token(nach_str, &zayavki[i].nach_min) ||
            !parse_time_token(konec_str, &zayavki[i].konec_min)) {
            fprintf(stderr, "Ошибка чтения заявки в строке %d.\n", i + 1);
            free(zayavki);
            return 1;
        }
    }

    st_kod = max_scheduled_talks(zayavki, (size_t)n, k, &otvet);
    free(zayavki);

    if (st_kod != STATUS_OK) {
        fprintf(stderr, "Ошибка расчёта расписания (код %d).\n", (int)st_kod);
        return 1;
    }

    printf("%d\n", otvet);
    return 0;
}
