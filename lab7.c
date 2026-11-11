#include <ctype.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
    STATUS_OK = 0,
    STATUS_INVALID_ARGUMENT,
    STATUS_FILE_ERROR,
    STATUS_MEMORY_ERROR,
    STATUS_NOT_FOUND
} StatusCode;

typedef struct {
    char familiya[64];
    char imya[64];
    char otchestvo[64];
    int den;
    int mesyac;
    int god;
    char pol;
    double dohod;
} Liver;

typedef struct LiverNode {
    Liver dannye;
    struct LiverNode *sled;
} LiverNode;

typedef enum {
    ACTION_ADD = 1,
    ACTION_DELETE,
    ACTION_MODIFY
} ActionType;

typedef struct {
    ActionType tip;
    Liver do_izm;
    Liver posle;
} Modification;

static int are_same_files(const char *put1, const char *put2)
{
    char poln1[4096];
    char poln2[4096];

    if (put1 == NULL || put2 == NULL) {
        return 0;
    }
    if (strcmp(put1, put2) == 0) {
        return 1;
    }
#ifdef _WIN32
    if (_fullpath(poln1, put1, sizeof(poln1)) != NULL &&
        _fullpath(poln2, put2, sizeof(poln2)) != NULL) {
        return _stricmp(poln1, poln2) == 0;
    }
#else
    if (realpath(put1, poln1) != NULL &&
        realpath(put2, poln2) != NULL) {
        return strcmp(poln1, poln2) == 0;
    }
#endif
    return 0;
}

static int is_latin_string(const char *vhod_str, int razresh_pust)
{
    size_t i;
    if (vhod_str == NULL) {
        return 0;
    }
    if (vhod_str[0] == '\0') {
        return razresh_pust ? 1 : 0;
    }
    for (i = 0; vhod_str[i] != '\0'; ++i) {
        if (!isalpha((unsigned char)vhod_str[i])) {
            return 0;
        }
    }
    return 1;
}

/* валидация даты рождения */
static int is_valid_date(int d, int m, int y)
{
    int dnei[] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
    if (y < 1900 || y > 2026 || m < 1 || m > 12) {
        return 0;
    }
    if ((y % 4 == 0 && y % 100 != 0) || (y % 400 == 0)) {
        dnei[1] = 29;
    }
    return d >= 1 && d <= dnei[m - 1];
}

/* валидация структуры жителя */
static int is_valid_liver(const Liver *lv, double tochnost_eps)
{
    if (lv == NULL || !isfinite(lv->dohod) || !isfinite(tochnost_eps) || tochnost_eps <= 0.0) {
        return 0;
    }
    if (!is_latin_string(lv->familiya, 0) ||
        !is_latin_string(lv->imya, 0) ||
        !is_latin_string(lv->otchestvo, 1)) {
        return 0;
    }
    if (!is_valid_date(lv->den, lv->mesyac, lv->god)) {
        return 0;
    }
    if (lv->pol != 'M' && lv->pol != 'W') {
        return 0;
    }
    return lv->dohod >= -tochnost_eps;
}

/* сравнение возраста: моложе у кого позже дата рождения */
static int compare_age_ascending(const Liver *a, const Liver *b)
{
    if (a->god != b->god) {
        return (b->god > a->god) - (b->god < a->god);
    }
    if (a->mesyac != b->mesyac) {
        return (b->mesyac > a->mesyac) - (b->mesyac < a->mesyac);
    }
    if (a->den != b->den) {
        return (b->den > a->den) - (b->den < a->den);
    }
    return strcmp(a->familiya, b->familiya);
}

static int livers_equal(const Liver *a, const Liver *b, double tochnost_eps)
{
    return strcmp(a->familiya, b->familiya) == 0 &&
           strcmp(a->imya, b->imya) == 0 &&
           strcmp(a->otchestvo, b->otchestvo) == 0 &&
           a->den == b->den &&
           a->mesyac == b->mesyac &&
           a->god == b->god &&
           a->pol == b->pol &&
           fabs(a->dohod - b->dohod) <= tochnost_eps;
}

/* вставка жителя с сортировкой по возрасту */
static StatusCode list_insert_sorted(LiverNode **golova, const Liver *lv, double tochnost_eps)
{
    LiverNode *uzel;
    LiverNode *tek;

    if (golova == NULL || !is_valid_liver(lv, tochnost_eps)) {
        return STATUS_INVALID_ARGUMENT;
    }

    uzel = (LiverNode *)malloc(sizeof(LiverNode));
    if (uzel == NULL) {
        return STATUS_MEMORY_ERROR;
    }
    uzel->dannye = *lv;
    uzel->sled = NULL;

    if (*golova == NULL || compare_age_ascending(lv, &(*golova)->dannye) <= 0) {
        uzel->sled = *golova;
        *golova = uzel;
        return STATUS_OK;
    }

    tek = *golova;
    while (tek->sled != NULL && compare_age_ascending(lv, &tek->sled->dannye) > 0) {
        tek = tek->sled;
    }
    uzel->sled = tek->sled;
    tek->sled = uzel;
    return STATUS_OK;
}

/* удаление первого совпадения из списка */
static StatusCode list_remove_exact(LiverNode **golova, const Liver *cel, double tochnost_eps)
{
    LiverNode *tek;
    LiverNode *pred = NULL;

    if (golova == NULL || cel == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }

    tek = *golova;
    while (tek != NULL) {
        if (livers_equal(&tek->dannye, cel, tochnost_eps)) {
            if (pred == NULL) {
                *golova = tek->sled;
            } else {
                pred->sled = tek->sled;
            }
            free(tek);
            return STATUS_OK;
        }
        pred = tek;
        tek = tek->sled;
    }
    return STATUS_NOT_FOUND;
}

static void list_free(LiverNode *golova)
{
    while (golova != NULL) {
        LiverNode *sled = golova->sled;
        free(golova);
        golova = sled;
    }
}

/* пуш модификации в стек истории для undo */
static StatusCode push_history(Modification **istoriya,
                               size_t *razmer,
                               size_t *emkost,
                               ActionType tip,
                               const Liver *do_izm,
                               const Liver *posle)
{
    if (*razmer >= *emkost) {
        size_t nov_emkost = (*emkost == 0U) ? 8U : (*emkost * 2U);
        Modification *nov_mas = (Modification *)realloc(*istoriya, nov_emkost * sizeof(Modification));
        if (nov_mas == NULL) {
            return STATUS_MEMORY_ERROR;
        }
        *istoriya = nov_mas;
        *emkost = nov_emkost;
    }
    (*istoriya)[*razmer].tip = tip;
    if (do_izm != NULL) {
        (*istoriya)[*razmer].do_izm = *do_izm;
    }
    if (posle != NULL) {
        (*istoriya)[*razmer].posle = *posle;
    }
    (*razmer)++;
    return STATUS_OK;
}

/* откат половины последних модификаций */
static StatusCode undo_half_modifications(LiverNode **golova,
                                          Modification *istoriya,
                                          size_t *razmer_ist,
                                          double tochnost_eps,
                                          size_t *kolvo_otmen_out)
{
    size_t k_otmene;
    size_t i;

    if (golova == NULL || razmer_ist == NULL || kolvo_otmen_out == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }

    k_otmene = *razmer_ist / 2U;
    *kolvo_otmen_out = k_otmene;

    for (i = 0U; i < k_otmene; ++i) {
        const Modification *izm = &istoriya[*razmer_ist - 1U - i];
        if (izm->tip == ACTION_ADD) {
            list_remove_exact(golova, &izm->posle, tochnost_eps);
        } else if (izm->tip == ACTION_DELETE) {
            list_insert_sorted(golova, &izm->do_izm, tochnost_eps);
        } else if (izm->tip == ACTION_MODIFY) {
            list_remove_exact(golova, &izm->posle, tochnost_eps);
            list_insert_sorted(golova, &izm->do_izm, tochnost_eps);
        }
    }

    *razmer_ist = 0U;
    return STATUS_OK;
}

/* чтение списка из файла (прочерк = пустое отчество) */
static StatusCode load_livers_from_file(const char *put_f,
                                        LiverNode **golova_out,
                                        double tochnost_eps)
{
    FILE *file;
    LiverNode *golova = NULL;
    char stroka[512];

    if (put_f == NULL || golova_out == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }

    file = fopen(put_f, "r");
    if (file == NULL) {
        return STATUS_FILE_ERROR;
    }

    while (fgets(stroka, sizeof(stroka), file) != NULL) {
        Liver lv;
        char otch_buf[64];
        char pol_str[16];
        int kolvo;

        if (stroka[0] == '\n' || stroka[0] == '\r' || stroka[0] == '\0') {
            continue;
        }

        memset(&lv, 0, sizeof(lv));
        kolvo = sscanf(stroka, "%63s %63s %63s %d %d %d %15s %lf",
                       lv.familiya, lv.imya, otch_buf,
                       &lv.den, &lv.mesyac, &lv.god,
                       pol_str, &lv.dohod);
        if (kolvo != 8) {
            list_free(golova);
            fclose(file);
            return STATUS_INVALID_ARGUMENT;
        }
        if (strcmp(otch_buf, "-") == 0) {
            lv.otchestvo[0] = '\0';
        } else {
            strncpy(lv.otchestvo, otch_buf, sizeof(lv.otchestvo) - 1U);
        }
        lv.pol = pol_str[0];

        if (list_insert_sorted(&golova, &lv, tochnost_eps) != STATUS_OK) {
            list_free(golova);
            fclose(file);
            return STATUS_INVALID_ARGUMENT;
        }
    }

    fclose(file);
    *golova_out = golova;
    return STATUS_OK;
}

/* выгрузка списка в файл */
static StatusCode save_livers_to_file(const char *put_f, const LiverNode *golova)
{
    FILE *file;
    const LiverNode *tek;

    if (put_f == NULL || put_f[0] == '\0') {
        return STATUS_INVALID_ARGUMENT;
    }

    file = fopen(put_f, "w");
    if (file == NULL) {
        return STATUS_FILE_ERROR;
    }

    for (tek = golova; tek != NULL; tek = tek->sled) {
        const Liver *lv = &tek->dannye;
        fprintf(file, "%s %s %s %02d %02d %04d %c %.2f\n",
                lv->familiya,
                lv->imya,
                lv->otchestvo[0] ? lv->otchestvo : "-",
                lv->den, lv->mesyac, lv->god,
                lv->pol, lv->dohod);
    }

    fclose(file);
    return STATUS_OK;
}

static void print_livers(const LiverNode *golova)
{
    const LiverNode *tek = golova;
    size_t ind = 1U;

    if (golova == NULL) {
        puts("Список жителей пуст.");
        return;
    }
    while (tek != NULL) {
        const Liver *lv = &tek->dannye;
        printf("  #%zu: %s %s %s | ДР: %02d.%02d.%04d | Пол: %c | Доход: %.2f\n",
               ind++,
               lv->familiya,
               lv->imya,
               lv->otchestvo[0] ? lv->otchestvo : "(нет отчества)",
               lv->den, lv->mesyac, lv->god,
               lv->pol, lv->dohod);
        tek = tek->sled;
    }
}

static int prompt_liver(Liver *vyhod, double tochnost_eps)
{
    char buf[256];
    char otch[64];
    char pol_sim[16];

    printf("Введите: Фамилия Имя Отчество(или -) День Месяц Год Пол(M/W) Доход:\n> ");
    if (fgets(buf, sizeof(buf), stdin) == NULL) {
        return 0;
    }
    memset(vyhod, 0, sizeof(*vyhod));
    if (sscanf(buf, "%63s %63s %63s %d %d %d %15s %lf",
               vyhod->familiya, vyhod->imya, otch,
               &vyhod->den, &vyhod->mesyac, &vyhod->god,
               pol_sim, &vyhod->dohod) != 8) {
        return 0;
    }
    if (strcmp(otch, "-") == 0) {
        vyhod->otchestvo[0] = '\0';
    } else {
        strncpy(vyhod->otchestvo, otch, sizeof(vyhod->otchestvo) - 1U);
    }
    vyhod->pol = pol_sim[0];
    return is_valid_liver(vyhod, tochnost_eps);
}

int main(int argc, char **argv)
{
    LiverNode *golova = NULL;
    Modification *istoriya = NULL;
    size_t razmer_ist = 0U;
    size_t emkost_ist = 0U;
    double tochnost_eps = 1e-6;
    char stroka[256];

    if (argc != 2) {
        fprintf(stderr, "Использование: %s <входной_файл>\n", argv[0]);
        return 1;
    }

    if (load_livers_from_file(argv[1], &golova, tochnost_eps) != STATUS_OK) {
        fprintf(stderr, "Ошибка чтения файла жителей.\n");
        return 1;
    }

    /* интерактивный цикл */
    for (;;) {
        printf("\n=== Меню жителей (модификаций с последнего сброса N = %zu) ===\n",
               razmer_ist);
        puts("1 - Показать всех жителей (в порядке увеличения возраста)");
        puts("2 - Найти жителя по фамилии");
        puts("3 - Добавить жителя");
        puts("4 - Удалить жителя по номеру в списке");
        puts("5 - Изменить жителя по номеру в списке");
        puts("6 - Выгрузить список в файл");
        puts("7 - Undo (отменить последние floor(N/2) модификаций)");
        puts("0 - Выход");
        printf("Выбор: ");

        if (fgets(stroka, sizeof(stroka), stdin) == NULL) {
            break;
        }
        stroka[strcspn(stroka, "\r\n")] = '\0';

        if (strcmp(stroka, "0") == 0) {
            break;
        } else if (strcmp(stroka, "1") == 0) {
            print_livers(golova);
        } else if (strcmp(stroka, "2") == 0) {
            const LiverNode *tek = golova;
            int nashli = 0;
            printf("Введите фамилию: ");
            if (fgets(stroka, sizeof(stroka), stdin) == NULL) break;
            stroka[strcspn(stroka, "\r\n")] = '\0';
            while (tek != NULL) {
                if (strcmp(tek->dannye.familiya, stroka) == 0) {
                    printf("  Найден: %s %s %s, %02d.%02d.%04d, %c, %.2f\n",
                           tek->dannye.familiya, tek->dannye.imya,
                           tek->dannye.otchestvo[0] ? tek->dannye.otchestvo : "-",
                           tek->dannye.den, tek->dannye.mesyac, tek->dannye.god,
                           tek->dannye.pol, tek->dannye.dohod);
                    nashli = 1;
                }
                tek = tek->sled;
            }
            if (!nashli) {
                puts("Житель не найден.");
            }
        } else if (strcmp(stroka, "3") == 0) {
            Liver lv;
            if (!prompt_liver(&lv, tochnost_eps)) {
                puts("Некорректные данные жителя.");
                continue;
            }
            if (list_insert_sorted(&golova, &lv, tochnost_eps) == STATUS_OK) {
                push_history(&istoriya, &razmer_ist, &emkost_ist, ACTION_ADD, NULL, &lv);
                puts("Житель добавлен.");
            }
        } else if (strcmp(stroka, "4") == 0 || strcmp(stroka, "5") == 0) {
            int is_modify = (strcmp(stroka, "5") == 0);
            size_t cel_ind = 0U;
            size_t tek_ind = 1U;
            LiverNode *tek = golova;

            print_livers(golova);
            printf("Введите номер жителя (#): ");
            if (fgets(stroka, sizeof(stroka), stdin) == NULL ||
                sscanf(stroka, "%zu", &cel_ind) != 1 || cel_ind == 0U) {
                puts("Некорректный номер.");
                continue;
            }
            while (tek != NULL && tek_ind < cel_ind) {
                tek = tek->sled;
                tek_ind++;
            }
            if (tek == NULL) {
                puts("Житель с таким номером не найден.");
                continue;
            }

            {
                Liver star_znach = tek->dannye;
                if (!is_modify) {
                    list_remove_exact(&golova, &star_znach, tochnost_eps);
                    push_history(&istoriya, &razmer_ist, &emkost_ist,
                                 ACTION_DELETE, &star_znach, NULL);
                    puts("Житель удалён.");
                } else {
                    Liver nov_znach;
                    if (!prompt_liver(&nov_znach, tochnost_eps)) {
                        puts("Некорректные новые данные.");
                        continue;
                    }
                    list_remove_exact(&golova, &star_znach, tochnost_eps);
                    list_insert_sorted(&golova, &nov_znach, tochnost_eps);
                    push_history(&istoriya, &razmer_ist, &emkost_ist,
                                 ACTION_MODIFY, &star_znach, &nov_znach);
                    puts("Данные жителя обновлены.");
                }
            }
        } else if (strcmp(stroka, "6") == 0) {
            printf("Введите путь к выходному файлу: ");
            if (fgets(stroka, sizeof(stroka), stdin) == NULL) break;
            stroka[strcspn(stroka, "\r\n")] = '\0';
            if (are_same_files(argv[1], stroka)) {
                puts("Выходной файл не должен совпадать с входным.");
                continue;
            }
            if (save_livers_to_file(stroka, golova) == STATUS_OK) {
                puts("Данные успешно сохранены.");
            } else {
                puts("Ошибка сохранения файла.");
            }
        } else if (strcmp(stroka, "7") == 0) {
            size_t otmeneno = 0U;
            undo_half_modifications(&golova, istoriya, &razmer_ist, tochnost_eps, &otmeneno);
            printf("Отменено модификаций: %zu.\n", otmeneno);
        } else {
            puts("Неизвестная команда.");
        }
    }

    free(istoriya);
    list_free(golova);
    return 0;
}
