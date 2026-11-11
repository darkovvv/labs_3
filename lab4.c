#include <ctype.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

typedef enum {
    STATUS_OK = 0,
    STATUS_INVALID_ARGUMENT,
    STATUS_MEMORY_ERROR,
    STATUS_NOT_FOUND,
    STATUS_ALREADY_EXISTS
} StatusCode;

typedef struct {
    char *dannye;
    int dlina_str;
} String;

typedef struct {
    String gorod;
    String ulitsa;
    unsigned int dom;
    String korpus;
    unsigned int kvartira;
    String indeks;
} Address;

typedef struct {
    Address poluchatel;
    double ves;
    String id_posylki;
    String sozdano;
    String dostavleno;
    int dostavlena;
} Mail;

typedef struct {
    Address *adres_otd;
    Mail *posylki;
    size_t kolvo;
    size_t emkost;
} Post;

/* создание стринги из сишной строки */
static StatusCode string_create(const char *si_str, String *vyhod)
{
    size_t dlina;

    if (si_str == NULL || vyhod == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }

    dlina = strlen(si_str);
    vyhod->dannye = (char *)malloc(dlina + 1U);
    if (vyhod->dannye == NULL) {
        vyhod->dlina_str = 0;
        return STATUS_MEMORY_ERROR;
    }

    memcpy(vyhod->dannye, si_str, dlina + 1U);
    vyhod->dlina_str = (int)dlina;
    return STATUS_OK;
}

/* очистка буфера стринги */
static void string_clear(String *vhod_str)
{
    if (vhod_str == NULL) {
        return;
    }
    free(vhod_str->dannye);
    vhod_str->dannye = NULL;
    vhod_str->dlina_str = 0;
}

/* порядок стринг: длина потом лексикографика */
static StatusCode string_compare_order(const String *perv,
                                       const String *vtor,
                                       int *rez_out)
{
    if (perv == NULL || vtor == NULL || rez_out == NULL ||
        perv->dannye == NULL || vtor->dannye == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }

    if (perv->dlina_str != vtor->dlina_str) {
        *rez_out = (perv->dlina_str > vtor->dlina_str) - (perv->dlina_str < vtor->dlina_str);
        return STATUS_OK;
    }

    *rez_out = strcmp(perv->dannye, vtor->dannye);
    return STATUS_OK;
}

static int string_lex_compare(const String *perv, const String *vtor)
{
    if (perv == NULL || vtor == NULL || perv->dannye == NULL || vtor->dannye == NULL) {
        return 0;
    }
    return strcmp(perv->dannye, vtor->dannye);
}

static StatusCode string_equals(const String *perv,
                                const String *vtor,
                                int *ravny_out)
{
    if (perv == NULL || vtor == NULL || ravny_out == NULL ||
        perv->dannye == NULL || vtor->dannye == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }

    *ravny_out = (perv->dlina_str == vtor->dlina_str &&
                     strcmp(perv->dannye, vtor->dannye) == 0);
    return STATUS_OK;
}

static StatusCode string_copy(const String *istochnik, String *adres_naznach)
{
    char *nov_buf;

    if (istochnik == NULL || adres_naznach == NULL || istochnik->dannye == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    if (istochnik == adres_naznach) {
        return STATUS_OK;
    }

    nov_buf = (char *)malloc((size_t)istochnik->dlina_str + 1U);
    if (nov_buf == NULL) {
        return STATUS_MEMORY_ERROR;
    }

    memcpy(nov_buf, istochnik->dannye, (size_t)istochnik->dlina_str + 1U);
    free(adres_naznach->dannye);
    adres_naznach->dannye = nov_buf;
    adres_naznach->dlina_str = istochnik->dlina_str;
    return STATUS_OK;
}

static StatusCode string_clone_new(const String *istochnik, String **klon_out)
{
    String * kop_ukaz;
    StatusCode st_kod;

    if (istochnik == NULL || klon_out == NULL || istochnik->dannye == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }

    kop_ukaz = (String *)malloc(sizeof(String));
    if (kop_ukaz == NULL) {
        return STATUS_MEMORY_ERROR;
    }
    kop_ukaz->dannye = NULL;
    kop_ukaz->dlina_str = 0;

    st_kod = string_create(istochnik->dannye, kop_ukaz);
    if (st_kod != STATUS_OK) {
        free(kop_ukaz);
        return st_kod;
    }

    *klon_out = kop_ukaz;
    return STATUS_OK;
}

static StatusCode string_concat(String *perv, const String *vtor)
{
    int obsh_dlina;
    char *nov_mas;

    if (perv == NULL || vtor == NULL ||
        perv->dannye == NULL || vtor->dannye == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }

    obsh_dlina = perv->dlina_str + vtor->dlina_str;
    nov_mas = (char *)realloc(perv->dannye, (size_t)obsh_dlina + 1U);
    if (nov_mas == NULL) {
        return STATUS_MEMORY_ERROR;
    }

    memcpy(nov_mas + perv->dlina_str, vtor->dannye, (size_t)vtor->dlina_str + 1U);
    perv->dannye = nov_mas;
    perv->dlina_str = obsh_dlina;
    return STATUS_OK;
}

static int is_exact_digits(const char *vhod_str, size_t ozhid_dlina)
{
    size_t i;

    if (vhod_str == NULL || strlen(vhod_str) != ozhid_dlina) {
        return 0;
    }
    for (i = 0; i < ozhid_dlina; ++i) {
        if (!isdigit((unsigned char)vhod_str[i])) {
            return 0;
        }
    }
    return 1;
}

/* парсинг даты в таймстемп */
static int parse_datetime(const char *tekst, time_t *ts_out)
{
    int den, mesyac, god, chas, minuta, vtor;
    int dnei_v_mesyace[] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
    struct tm tm_znach;
    size_t i;

    if (tekst == NULL || ts_out == NULL || strlen(tekst) != 19U) {
        return 0;
    }

    for (i = 0; i < 19U; ++i) {
        if (i == 2U || i == 5U || i == 13U || i == 16U) {
            if (tekst[i] != ':') {
                return 0;
            }
        } else if (i == 10U) {
            if (tekst[i] != ' ') {
                return 0;
            }
        } else if (!isdigit((unsigned char)tekst[i])) {
            return 0;
        }
    }

    if (sscanf(tekst, "%2d:%2d:%4d %2d:%2d:%2d",
               &den, &mesyac, &god, &chas, &minuta, &vtor) != 6) {
        return 0;
    }

    if (god < 1970 || god > 2100 || mesyac < 1 || mesyac > 12 ||
        chas < 0 || chas > 23 || minuta < 0 || minuta > 59 ||
        vtor < 0 || vtor > 59) {
        return 0;
    }

    if ((god % 4 == 0 && god % 100 != 0) || (god % 400 == 0)) {
        dnei_v_mesyace[1] = 29;
    }
    if (den < 1 || den > dnei_v_mesyace[mesyac - 1]) {
        return 0;
    }

    memset(&tm_znach, 0, sizeof(tm_znach));
    tm_znach.tm_mday = den;
    tm_znach.tm_mon = mesyac - 1;
    tm_znach.tm_year = god - 1900;
    tm_znach.tm_hour = chas;
    tm_znach.tm_min = minuta;
    tm_znach.tm_sec = vtor;
    tm_znach.tm_isdst = -1;

    *ts_out = mktime(&tm_znach);
    return *ts_out != (time_t)-1;
}

static void address_clear(Address *adres)
{
    if (adres == NULL) {
        return;
    }
    string_clear(&adres->gorod);
    string_clear(&adres->ulitsa);
    string_clear(&adres->korpus);
    string_clear(&adres->indeks);
}

static void mail_clear(Mail *mail)
{
    if (mail == NULL) {
        return;
    }
    address_clear(&mail->poluchatel);
    string_clear(&mail->id_posylki);
    string_clear(&mail->sozdano);
    string_clear(&mail->dostavleno);
}

static int compare_mail_zip_id(const void *lev_ukaz, const void *prav_ukaz)
{
    const Mail *lev = (const Mail *)lev_ukaz;
    const Mail *prav = (const Mail *)prav_ukaz;
    int indeks_srav = string_lex_compare(&lev->poluchatel.indeks, &prav->poluchatel.indeks);

    if (indeks_srav != 0) {
        return indeks_srav;
    }
    return string_lex_compare(&lev->id_posylki, &prav->id_posylki);
}

static int compare_mail_created_time(const void *lev_ukaz, const void *prav_ukaz)
{
    const Mail * const *lev = (const Mail * const *)lev_ukaz;
    const Mail * const *prav = (const Mail * const *)prav_ukaz;
    time_t t1 = 0;
    time_t t2 = 0;

    parse_datetime((*lev)->sozdano.dannye, &t1);
    parse_datetime((*prav)->sozdano.dannye, &t2);
    if (t1 != t2) {
        return (t1 > t2) - (t1 < t2);
    }
    return string_lex_compare(&(*lev)->id_posylki, &(*prav)->id_posylki);
}

/* создание посылки с валидацией полей */
static StatusCode mail_create(const char *gorod,
                              const char *ulitsa,
                              unsigned int dom,
                              const char *korpus,
                              unsigned int kvartira,
                              const char *indeks,
                              double ves,
                              double tochnost_eps,
                              const char *id_posylki,
                              const char *sozdano,
                              const char *dostavleno,
                              int dostavlena,
                              Mail *vyhod)
{
    time_t sozdano_ts;
    time_t dostavleno_ts;

    if (gorod == NULL || ulitsa == NULL || korpus == NULL || indeks == NULL ||
        id_posylki == NULL || sozdano == NULL || dostavleno == NULL || vyhod == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }

    if (gorod[0] == '\0' || ulitsa[0] == '\0' || dom == 0U || kvartira == 0U ||
        !is_exact_digits(indeks, 6U) || !is_exact_digits(id_posylki, 14U) ||
        !isfinite(ves) || !isfinite(tochnost_eps) || tochnost_eps <= 0.0 || ves < -tochnost_eps ||
        !parse_datetime(sozdano, &sozdano_ts) ||
        !parse_datetime(dostavleno, &dostavleno_ts) ||
        dostavleno_ts < sozdano_ts) {
        return STATUS_INVALID_ARGUMENT;
    }

    memset(vyhod, 0, sizeof(*vyhod));
    vyhod->poluchatel.dom = dom;
    vyhod->poluchatel.kvartira = kvartira;
    vyhod->ves = ves < 0.0 ? 0.0 : ves;
    vyhod->dostavlena = dostavlena ? 1 : 0;

    if (string_create(gorod, &vyhod->poluchatel.gorod) != STATUS_OK ||
        string_create(ulitsa, &vyhod->poluchatel.ulitsa) != STATUS_OK ||
        string_create(korpus, &vyhod->poluchatel.korpus) != STATUS_OK ||
        string_create(indeks, &vyhod->poluchatel.indeks) != STATUS_OK ||
        string_create(id_posylki, &vyhod->id_posylki) != STATUS_OK ||
        string_create(sozdano, &vyhod->sozdano) != STATUS_OK ||
        string_create(dostavleno, &vyhod->dostavleno) != STATUS_OK) {
        mail_clear(vyhod);
        return STATUS_MEMORY_ERROR;
    }

    return STATUS_OK;
}

/* добавление посылки с сохранением сортировки */
static StatusCode post_add_mail(Post *post, const Mail *mail)
{
    size_t i;

    if (post == NULL || mail == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }

    for (i = 0; i < post->kolvo; ++i) {
        int eq = 0;
        string_equals(&post->posylki[i].id_posylki, &mail->id_posylki, &eq);
        if (eq) {
            return STATUS_ALREADY_EXISTS;
        }
    }

    if (post->kolvo >= post->emkost) {
        size_t nov_emkost = post->emkost == 0U ? 8U : post->emkost * 2U;
        Mail *nov_mas = (Mail *)realloc(post->posylki, nov_emkost * sizeof(Mail));
        if (nov_mas == NULL) {
            return STATUS_MEMORY_ERROR;
        }
        post->posylki = nov_mas;
        post->emkost = nov_emkost;
    }

    post->posylki[post->kolvo++] = *mail;
    qsort(post->posylki, post->kolvo, sizeof(Mail), compare_mail_zip_id);
    return STATUS_OK;
}

static StatusCode post_find_by_id(const Post *post,
                                  const char *id_posylki,
                                  size_t *ind_out)
{
    size_t i;

    if (post == NULL || id_posylki == NULL || ind_out == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }

    for (i = 0; i < post->kolvo; ++i) {
        if (strcmp(post->posylki[i].id_posylki.dannye, id_posylki) == 0) {
            *ind_out = i;
            return STATUS_OK;
        }
    }
    return STATUS_NOT_FOUND;
}

/* удаление посылки со сдвигом массива */
static StatusCode post_remove_by_id(Post *post, const char *id_posylki)
{
    size_t ind = 0U;
    size_t i;
    StatusCode st_kod;

    st_kod = post_find_by_id(post, id_posylki, &ind);
    if (st_kod != STATUS_OK) {
        return st_kod;
    }

    mail_clear(&post->posylki[ind]);
    for (i = ind; i + 1U < post->kolvo; ++i) {
        post->posylki[i] = post->posylki[i + 1U];
    }
    post->kolvo--;
    return STATUS_OK;
}

static void print_mail(const Mail *mail)
{
    printf("  ID: %s | Индекс: %s | %s, ул. %s, д. %u, корп. %s, кв. %u\n",
           mail->id_posylki.dannye,
           mail->poluchatel.indeks.dannye,
           mail->poluchatel.gorod.dannye,
           mail->poluchatel.ulitsa.dannye,
           mail->poluchatel.dom,
           mail->poluchatel.korpus.dannye[0] ? mail->poluchatel.korpus.dannye : "-",
           mail->poluchatel.kvartira);
    printf("     Вес: %.2f кг | Создано: %s | Срок/вручение: %s | Статус: %s\n",
           mail->ves,
           mail->sozdano.dannye,
           mail->dostavleno.dannye,
           mail->dostavlena ? "доставлено" : "в пути");
}

/* сбор доставленных или просроченных посылок */
static StatusCode post_collect_filtered(const Post *post,
                                        int nuzhny_dost,
                                        time_t tek_vremya,
                                        const Mail ***spisok_out,
                                        size_t *kolvo_out)
{
    const Mail **massiv = NULL;
    size_t sovpalo = 0U;
    size_t i;

    if (post == NULL || spisok_out == NULL || kolvo_out == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }

    *spisok_out = NULL;
    *kolvo_out = 0U;
    if (post->kolvo == 0U) {
        return STATUS_OK;
    }

    massiv = (const Mail **)malloc(post->kolvo * sizeof(const Mail *));
    if (massiv == NULL) {
        return STATUS_MEMORY_ERROR;
    }

    for (i = 0; i < post->kolvo; ++i) {
        time_t srok = 0;
        parse_datetime(post->posylki[i].dostavleno.dannye, &srok);

        if (nuzhny_dost) {
            if (post->posylki[i].dostavlena) {
                massiv[sovpalo++] = &post->posylki[i];
            }
        } else {
            if (!post->posylki[i].dostavlena && srok < tek_vremya) {
                massiv[sovpalo++] = &post->posylki[i];
            }
        }
    }

    if (sovpalo > 1U) {
        qsort(massiv, sovpalo, sizeof(const Mail *), compare_mail_created_time);
    }

    *spisok_out = massiv;
    *kolvo_out = sovpalo;
    return STATUS_OK;
}

static int read_line(char *buf, size_t razmer)
{
    size_t dlina;
    if (fgets(buf, (int)razmer, stdin) == NULL) {
        return 0;
    }
    dlina = strlen(buf);
    while (dlina > 0U && (buf[dlina - 1U] == '\n' || buf[dlina - 1U] == '\r')) {
        buf[--dlina] = '\0';
    }
    return 1;
}

static void demo_string_type(void)
{
    String s1 = {NULL, 0};
    String s2 = {NULL, 0};
    String s3 = {NULL, 0};
    String *klon = NULL;
    int rez_srav = 0;
    int eq = 0;

    puts("=== демка п. 4.1 (тип строки) ===");
    string_create("Hello", &s1);
    string_create("World!", &s2);
    printf("s1 = \"%s\" (len=%d), s2 = \"%s\" (len=%d)\n",
           s1.dannye, s1.dlina_str, s2.dannye, s2.dlina_str);

    string_compare_order(&s1, &s2, &rez_srav);
    string_equals(&s1, &s2, &eq);
    printf("Сравнение порядка s1 и s2: %d, эквивалентность: %d\n", rez_srav, eq);

    string_create(" Initial", &s3);
    string_copy(&s1, &s3);
    printf("После копирования s1 -> s3: \"%s\"\n", s3.dannye);

    if (string_clone_new(&s2, &klon) == STATUS_OK && klon != NULL) {
        printf("Клон s2 в куче: \"%s\" (len=%d)\n", klon->dannye, klon->dlina_str);
        string_clear(klon);
        free(klon);
    }

    string_concat(&s1, &s2);
    printf("После конкатенации s1 + s2: \"%s\" (len=%d)\n\n", s1.dannye, s1.dlina_str);

    string_clear(&s1);
    string_clear(&s2);
    string_clear(&s3);
}

int main(void)
{
    Address otdelenie = {{NULL, 0}, {NULL, 0}, 1U, {NULL, 0}, 1U, {NULL, 0}};
    Post post;
    Mail obrazec;
    char stroka[256];
    size_t i;

    demo_string_type();

    string_create("Москва", &otdelenie.gorod);
    string_create("Волоколамское шоссе", &otdelenie.ulitsa);
    otdelenie.dom = 4U;
    string_create("1", &otdelenie.korpus);
    otdelenie.kvartira = 10U;
    string_create("125993", &otdelenie.indeks);

    post.adres_otd = &otdelenie;
    post.posylki = NULL;
    post.kolvo = 0U;
    post.emkost = 0U;

    if (mail_create("Москва", "Тверская", 12U, "А", 45U, "125009",
                    1.5, 1e-9, "10000000000002",
                    "10:10:2025 12:00:00", "15:10:2025 18:00:00",
                    1, &obrazec) == STATUS_OK) {
        post_add_mail(&post, &obrazec);
    }
    if (mail_create("Казань", "Баумана", 7U, "", 3U, "420111",
                    0.8, 1e-9, "10000000000001",
                    "05:10:2025 09:30:00", "20:10:2025 12:00:00",
                    0, &obrazec) == STATUS_OK) {
        post_add_mail(&post, &obrazec);
    }
    if (mail_create("Москва", "Арбат", 5U, "2", 19U, "125009",
                    3.2, 1e-9, "10000000000001",
                    "01:10:2025 08:15:00", "12:10:2025 15:00:00",
                    1, &obrazec) == STATUS_OK) {
        post_add_mail(&post, &obrazec);
    }

    /* интерактивное меню */
    for (;;) {
        puts("=== Меню отделения почты (п. 4.2) ===");
        puts("1 - Показать все посылки (отсортированы по индексу и ID)");
        puts("2 - Добавить посылку");
        puts("3 - Удалить посылку по ID");
        puts("4 - Найти посылку по ID");
        puts("5 - Показать доставленные посылки (по времени создания)");
        puts("6 - Показать просроченные посылки на текущее системное время");
        puts("0 - Выход");
        printf("Выбор: ");

        if (!read_line(stroka, sizeof(stroka))) {
            break;
        }

        if (strcmp(stroka, "0") == 0) {
            break;
        } else if (strcmp(stroka, "1") == 0) {
            if (post.kolvo == 0U) {
                puts("Список посылок пуст.");
            } else {
                for (i = 0; i < post.kolvo; ++i) {
                    print_mail(&post.posylki[i]);
                }
            }
        } else if (strcmp(stroka, "2") == 0) {
            char gorod[128], ulitsa[128], korpus[64], indeks[32];
            char id_posylki[32], sozdano[64], dostavleno[64], vrem[64];
            unsigned int dom = 0U, kvartira = 0U;
            double ves = 0.0;
            int flag_dost = 0;
            Mail m;

            printf("Город: ");
            if (!read_line(gorod, sizeof(gorod))) break;
            printf("Улица: ");
            if (!read_line(ulitsa, sizeof(ulitsa))) break;
            printf("Номер дома: ");
            if (!read_line(vrem, sizeof(vrem)) || sscanf(vrem, "%u", &dom) != 1) {
                puts("Некорректный номер дома.");
                continue;
            }
            printf("Корпус (можно пусто): ");
            if (!read_line(korpus, sizeof(korpus))) break;
            printf("Номер квартиры: ");
            if (!read_line(vrem, sizeof(vrem)) || sscanf(vrem, "%u", &kvartira) != 1) {
                puts("Некорректный номер квартиры.");
                continue;
            }
            printf("Индекс (6 цифр): ");
            if (!read_line(indeks, sizeof(indeks))) break;
            printf("Вес посылки: ");
            if (!read_line(vrem, sizeof(vrem)) || sscanf(vrem, "%lf", &ves) != 1) {
                puts("Некорректный вес.");
                continue;
            }
            printf("Почтовый ID (14 цифр): ");
            if (!read_line(id_posylki, sizeof(id_posylki))) break;
            printf("Время создания (dd:MM:yyyy hh:mm:ss): ");
            if (!read_line(sozdano, sizeof(sozdano))) break;
            printf("Время вручения/срок (dd:MM:yyyy hh:mm:ss): ");
            if (!read_line(dostavleno, sizeof(dostavleno))) break;
            printf("Доставлено? (1 - да, 0 - нет): ");
            if (!read_line(vrem, sizeof(vrem)) || sscanf(vrem, "%d", &flag_dost) != 1) {
                puts("Некорректный статус.");
                continue;
            }

            if (mail_create(gorod, ulitsa, dom, korpus, kvartira, indeks,
                            ves, 1e-9, id_posylki, sozdano, dostavleno,
                            flag_dost, &m) != STATUS_OK) {
                puts("Ошибка валидации данных посылки.");
            } else {
                StatusCode st = post_add_mail(&post, &m);
                if (st == STATUS_OK) {
                    puts("Посылка успешно добавлена.");
                } else {
                    mail_clear(&m);
                    puts("Ошибка добавления (возможно, ID уже существует).");
                }
            }
        } else if (strcmp(stroka, "3") == 0) {
            printf("Введите 14-значный ID для удаления: ");
            if (!read_line(stroka, sizeof(stroka))) break;
            if (post_remove_by_id(&post, stroka) == STATUS_OK) {
                puts("Посылка удалена.");
            } else {
                puts("Посылка с таким ID не найдена.");
            }
        } else if (strcmp(stroka, "4") == 0) {
            size_t ind = 0U;
            printf("Введите 14-значный ID для поиска: ");
            if (!read_line(stroka, sizeof(stroka))) break;
            if (post_find_by_id(&post, stroka, &ind) == STATUS_OK) {
                print_mail(&post.posylki[ind]);
            } else {
                puts("Посылка не найдена.");
            }
        } else if (strcmp(stroka, "5") == 0 || strcmp(stroka, "6") == 0) {
            int nuzhny_dost = (strcmp(stroka, "5") == 0);
            const Mail **otfiltr = NULL;
            size_t kolvo_otfiltr = 0U;
            time_t seychas = time(NULL);

            if (post_collect_filtered(&post, nuzhny_dost, seychas,
                                      &otfiltr, &kolvo_otfiltr) == STATUS_OK) {
                if (kolvo_otfiltr == 0U) {
                    puts("Подходящих отправлений нет.");
                } else {
                    for (i = 0; i < kolvo_otfiltr; ++i) {
                        print_mail(otfiltr[i]);
                    }
                }
                free((void *)otfiltr);
            }
        } else {
            puts("Неизвестный пункт меню.");
        }
        putchar('\n');
    }

    /* очистка динамики перед выходом */
    for (i = 0; i < post.kolvo; ++i) {
        mail_clear(&post.posylki[i]);
    }
    free(post.posylki);
    address_clear(&otdelenie);

    return 0;
}
