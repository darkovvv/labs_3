#include <ctype.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

typedef enum {
    STATUS_OK = 0,
    STATUS_INVALID_ARGUMENT,
    STATUS_FILE_ERROR,
    STATUS_MEMORY_ERROR
} StatusCode;

typedef struct StopNode {
    double x;
    double y;
    time_t vremya_pribyt;
    time_t vremya_otpravl;
    char marker; /* тип остановки */
    struct StopNode *sled;
} StopNode;

typedef struct VehicleNode {
    char id_ts[64];
    StopNode *golova_ost;
    struct VehicleNode *sled;
} VehicleNode;

/* парсинг таймстемпа из строки */
static int parse_dot_datetime(const char *vhod_str, time_t *vremya_out)
{
    int d, m, y, hh, mm, ss;
    struct tm tm_znach;

    if (vhod_str == NULL || vremya_out == NULL) {
        return 0;
    }
    if (sscanf(vhod_str, "%2d.%2d.%4d %2d:%2d:%2d", &d, &m, &y, &hh, &mm, &ss) != 6) {
        return 0;
    }
    if (y < 1970 || m < 1 || m > 12 || d < 1 || d > 31 ||
        hh < 0 || hh > 23 || mm < 0 || mm > 59 || ss < 0 || ss > 59) {
        return 0;
    }

    memset(&tm_znach, 0, sizeof(tm_znach));
    tm_znach.tm_mday = d;
    tm_znach.tm_mon = m - 1;
    tm_znach.tm_year = y - 1900;
    tm_znach.tm_hour = hh;
    tm_znach.tm_min = mm;
    tm_znach.tm_sec = ss;
    tm_znach.tm_isdst = -1;

    *vremya_out = mktime(&tm_znach);
    return *vremya_out != (time_t)-1;
}

/* поиск или создание узла транспорта */
static VehicleNode *get_or_create_vehicle(VehicleNode **golova, const char *id_ts)
{
    VehicleNode *tek = *golova;
    VehicleNode *sozdan;

    while (tek != NULL) {
        if (strcmp(tek->id_ts, id_ts) == 0) {
            return tek;
        }
        tek = tek->sled;
    }

    sozdan = (VehicleNode *)malloc(sizeof(VehicleNode));
    if (sozdan == NULL) {
        return NULL;
    }
    strncpy(sozdan->id_ts, id_ts, sizeof(sozdan->id_ts) - 1U);
    sozdan->id_ts[sizeof(sozdan->id_ts) - 1U] = '\0';
    sozdan->golova_ost = NULL;
    sozdan->sled = *golova;
    *golova = sozdan;
    return sozdan;
}

/* вставка остановки с сортировкой по времени */
static StatusCode insert_stop_sorted(VehicleNode *ts,
                                     double x,
                                     double y,
                                     time_t pribytie,
                                     time_t otpravl,
                                     char marker)
{
    StopNode *uzel;
    StopNode *tek;

    if (ts == NULL || otpravl < pribytie) {
        return STATUS_INVALID_ARGUMENT;
    }

    uzel = (StopNode *)malloc(sizeof(StopNode));
    if (uzel == NULL) {
        return STATUS_MEMORY_ERROR;
    }
    uzel->x = x;
    uzel->y = y;
    uzel->vremya_pribyt = pribytie;
    uzel->vremya_otpravl = otpravl;
    uzel->marker = marker;
    uzel->sled = NULL;

    if (ts->golova_ost == NULL ||
        pribytie < ts->golova_ost->vremya_pribyt) {
        uzel->sled = ts->golova_ost;
        ts->golova_ost = uzel;
        return STATUS_OK;
    }

    tek = ts->golova_ost;
    while (tek->sled != NULL && tek->sled->vremya_pribyt <= pribytie) {
        tek = tek->sled;
    }
    uzel->sled = tek->sled;
    tek->sled = uzel;
    return STATUS_OK;
}

/* очистка списка списков */
static void free_vehicles(VehicleNode *golova)
{
    while (golova != NULL) {
        VehicleNode *sled_ts = golova->sled;
        StopNode *ostanovka = golova->golova_ost;
        while (ostanovka != NULL) {
            StopNode *sled_ost = ostanovka->sled;
            free(ostanovka);
            ostanovka = sled_ost;
        }
        free(golova);
        golova = sled_ts;
    }
}

static StatusCode load_stop_file(const char *put_f, VehicleNode **golova_ts)
{
    FILE *file;
    double x = 0.0;
    double y = 0.0;

    if (put_f == NULL || golova_ts == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }

    file = fopen(put_f, "r");
    if (file == NULL) {
        return STATUS_FILE_ERROR;
    }

    if (fscanf(file, "%lf %lf", &x, &y) != 2) {
        fclose(file);
        return STATUS_INVALID_ARGUMENT;
    }

    for (;;) {
        char id[64];
        char data_pr[16], vremya_pr[16];
        char data_ot[16], vremya_ot[16];
        char marker_str[16];
        char pr_poln[32], ot_poln[32];
        time_t pr_ts, ot_ts;
        char marker;
        VehicleNode *v;
        int kolvo_prochit;

        kolvo_prochit = fscanf(file, "%63s %15s %15s %15s %15s %15s",
                            id, data_pr, vremya_pr, data_ot, vremya_ot, marker_str);
        if (kolvo_prochit == EOF) {
            break;
        }
        if (kolvo_prochit != 6) {
            fclose(file);
            return STATUS_INVALID_ARGUMENT;
        }

        snprintf(pr_poln, sizeof(pr_poln), "%s %s", data_pr, vremya_pr);
        snprintf(ot_poln, sizeof(ot_poln), "%s %s", data_ot, vremya_ot);
        marker = (char)toupper((unsigned char)marker_str[0]);

        if (!parse_dot_datetime(pr_poln, &pr_ts) ||
            !parse_dot_datetime(ot_poln, &ot_ts) ||
            (marker != 'S' && marker != 'I' && marker != 'E')) {
            fclose(file);
            return STATUS_INVALID_ARGUMENT;
        }

        v = get_or_create_vehicle(golova_ts, id);
        if (v == NULL ||
            insert_stop_sorted(v, x, y, pr_ts, ot_ts, marker) != STATUS_OK) {
            fclose(file);
            return STATUS_MEMORY_ERROR;
        }
    }

    fclose(file);
    return STATUS_OK;
}

/* подсчет метрик по одному транспорту */
static void compute_vehicle_stats(const VehicleNode *v,
                                  int *kolvo_marsh_out,
                                  double *obsh_dist_out,
                                  double *max_dist_marsh_out,
                                  double *min_dist_marsh_out,
                                  double *max_ozhid_ost_out,
                                  double *min_ozhid_ost_out,
                                  double *obsh_prostoy_out)
{
    const StopNode *tek = v->golova_ost;
    const StopNode *pred = NULL;
    int marshruty = 0;
    double obsh_dist = 0.0;
    double tek_dist_marsh = 0.0;
    double max_marsh = 0.0;
    double min_marsh = -1.0;
    double max_ozhid = 0.0;
    double min_ozhid = -1.0;
    double obsh_prostoy = 0.0;

    while (tek != NULL) {
        double ozhid_sek = difftime(tek->vremya_otpravl, tek->vremya_pribyt);
        obsh_prostoy += ozhid_sek;

        if (min_ozhid < 0.0 || ozhid_sek < min_ozhid) {
            min_ozhid = ozhid_sek;
        }
        if (ozhid_sek > max_ozhid) {
            max_ozhid = ozhid_sek;
        }

        if (tek->marker == 'S') {
            tek_dist_marsh = 0.0;
        } else if (pred != NULL) {
            double dx = tek->x - pred->x;
            double dy = tek->y - pred->y;
            double shag = sqrt(dx * dx + dy * dy);
            obsh_dist += shag;
            tek_dist_marsh += shag;
        }

        if (tek->marker == 'E') {
            marshruty++;
            if (min_marsh < 0.0 || tek_dist_marsh < min_marsh) {
                min_marsh = tek_dist_marsh;
            }
            if (tek_dist_marsh > max_marsh) {
                max_marsh = tek_dist_marsh;
            }
            tek_dist_marsh = 0.0;
        }

        pred = tek;
        tek = tek->sled;
    }

    *kolvo_marsh_out = marshruty;
    *obsh_dist_out = obsh_dist;
    *max_dist_marsh_out = max_marsh;
    *min_dist_marsh_out = min_marsh < 0.0 ? 0.0 : min_marsh;
    *max_ozhid_ost_out = max_ozhid;
    *min_ozhid_ost_out = min_ozhid < 0.0 ? 0.0 : min_ozhid;
    *obsh_prostoy_out = obsh_prostoy;
}

/* сводка по критериям из задания */
static StatusCode evaluate_fleet(const VehicleNode *golova, double tochnost_eps)
{
    const VehicleNode *tek;
    const VehicleNode *max_marshrutov = NULL, *min_marshrutov = NULL;
    const VehicleNode *dlin_put = NULL, *korot_put = NULL;
    const VehicleNode *dlin_marsh = NULL, *korot_marsh = NULL;
    const VehicleNode *dlin_ost = NULL, *korot_ost = NULL;
    const VehicleNode *max_prost_ts = NULL;

    int lu_max_marsh = -1, lu_min_marsh = -1;
    double lu_dlin_put = -1.0, lu_korot_put = -1.0;
    double lu_dlin_marsh = -1.0, lu_korot_marsh = -1.0;
    double lu_dlin_ost = -1.0, lu_korot_ost = -1.0;
    double lu_prostoy = -1.0;

    if (golova == NULL || !isfinite(tochnost_eps) || tochnost_eps <= 0.0) {
        return STATUS_INVALID_ARGUMENT;
    }

    for (tek = golova; tek != NULL; tek = tek->sled) {
        int marshruty;
        double obsh_d, max_marsh_k, min_marsh_k, max_ost_k, min_ost_k, prostoy;

        compute_vehicle_stats(tek, &marshruty, &obsh_d, &max_marsh_k, &min_marsh_k,
                              &max_ost_k, &min_ost_k, &prostoy);

        if (max_marshrutov == NULL || marshruty > lu_max_marsh) {
            lu_max_marsh = marshruty;
            max_marshrutov = tek;
        }
        if (min_marshrutov == NULL || marshruty < lu_min_marsh) {
            lu_min_marsh = marshruty;
            min_marshrutov = tek;
        }
        if (dlin_put == NULL || obsh_d - lu_dlin_put > tochnost_eps) {
            lu_dlin_put = obsh_d;
            dlin_put = tek;
        }
        if (korot_put == NULL || lu_korot_put - obsh_d > tochnost_eps) {
            lu_korot_put = obsh_d;
            korot_put = tek;
        }
        if (dlin_marsh == NULL || max_marsh_k - lu_dlin_marsh > tochnost_eps) {
            lu_dlin_marsh = max_marsh_k;
            dlin_marsh = tek;
        }
        if (korot_marsh == NULL || lu_korot_marsh - min_marsh_k > tochnost_eps) {
            lu_korot_marsh = min_marsh_k;
            korot_marsh = tek;
        }
        if (dlin_ost == NULL || max_ost_k - lu_dlin_ost > tochnost_eps) {
            lu_dlin_ost = max_ost_k;
            dlin_ost = tek;
        }
        if (korot_ost == NULL || lu_korot_ost - min_ost_k > tochnost_eps) {
            lu_korot_ost = min_ost_k;
            korot_ost = tek;
        }
        if (max_prost_ts == NULL || prostoy - lu_prostoy > tochnost_eps) {
            lu_prostoy = prostoy;
            max_prost_ts = tek;
        }
    }

    printf("Больше всего маршрутов: %s (%d)\n", max_marshrutov->id_ts, lu_max_marsh);
    printf("Меньше всего маршрутов: %s (%d)\n", min_marshrutov->id_ts, lu_min_marsh);
    printf("Самый длинный путь:     %s (%.2f)\n", dlin_put->id_ts, lu_dlin_put);
    printf("Самый короткий путь:    %s (%.2f)\n", korot_put->id_ts, lu_korot_put);
    printf("Самый длинный маршрут:  %s (%.2f)\n", dlin_marsh->id_ts, lu_dlin_marsh);
    printf("Самый короткий маршрут: %s (%.2f)\n", korot_marsh->id_ts, lu_korot_marsh);
    printf("Самая долгая остановка: %s (%.0f сек)\n", dlin_ost->id_ts, lu_dlin_ost);
    printf("Самая короткая ост-ка:  %s (%.0f сек)\n", korot_ost->id_ts, lu_korot_ost);
    printf("Наибольший простой:     %s (%.0f сек)\n", max_prost_ts->id_ts, lu_prostoy);

    return STATUS_OK;
}

int main(int argc, char **argv)
{
    VehicleNode *tr_sredstva = NULL;
    char stroka[128];
    int i;

    if (argc < 2) {
        fprintf(stderr, "Использование: %s <файл_остановки_1> [файл_2 ...]\n", argv[0]);
        return 1;
    }

    for (i = 1; i < argc; ++i) {
        StatusCode st = load_stop_file(argv[i], &tr_sredstva);
        if (st != STATUS_OK) {
            fprintf(stderr, "Ошибка чтения файла «%s» (код %d).\n", argv[i], (int)st);
            free_vehicles(tr_sredstva);
            return 1;
        }
    }

    if (tr_sredstva == NULL) {
        puts("Нет данных о транспортных средствах.");
        return 0;
    }

    /* интерактивный диалог запросов */
    for (;;) {
        puts("\n=== Меню анализа городского транспорта ===");
        puts("1 - Вывести полную статистику по всем критериям поиска");
        puts("2 - Показать все загруженные ТС и их остановки");
        puts("0 - Выход");
        printf("Выбор: ");

        if (fgets(stroka, sizeof(stroka), stdin) == NULL) {
            break;
        }
        stroka[strcspn(stroka, "\r\n")] = '\0';

        if (strcmp(stroka, "0") == 0) {
            break;
        } else if (strcmp(stroka, "1") == 0) {
            evaluate_fleet(tr_sredstva, 1e-9);
        } else if (strcmp(stroka, "2") == 0) {
            const VehicleNode *v;
            for (v = tr_sredstva; v != NULL; v = v->sled) {
                const StopNode *s;
                printf("ТС %s:\n", v->id_ts);
                for (s = v->golova_ost; s != NULL; s = s->sled) {
                    printf("  [%c] (%.1f, %.1f) приб=%lld отпр=%lld\n",
                           s->marker, s->x, s->y,
                           (long long)s->vremya_pribyt,
                           (long long)s->vremya_otpravl);
                }
            }
        } else {
            puts("Неизвестная команда.");
        }
    }

    free_vehicles(tr_sredstva);
    return 0;
}
