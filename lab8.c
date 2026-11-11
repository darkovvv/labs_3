#include <ctype.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
    STATUS_OK = 0,
    STATUS_INVALID_ARGUMENT,
    STATUS_DIVISION_BY_ZERO,
    STATUS_SYNTAX_ERROR,
    STATUS_FILE_ERROR,
    STATUS_MEMORY_ERROR
} StatusCode;

typedef struct TermNode {
    long long koef;
    int stepen;
    struct TermNode *sled;
} TermNode;

typedef struct {
    TermNode *golova;
} Polynomial;

static void poly_init(Polynomial *p)
{
    if (p != NULL) {
        p->golova = NULL;
    }
}

static void poly_free(Polynomial *p)
{
    TermNode *tek;
    if (p == NULL) {
        return;
    }
    tek = p->golova;
    while (tek != NULL) {
        TermNode *sled = tek->sled;
        free(tek);
        tek = sled;
    }
    p->golova = NULL;
}

/* добавление монома с приведением подобных */
static StatusCode poly_add_term(Polynomial *p, long long koef, int stepen)
{
    TermNode *tek;
    TermNode *pred = NULL;
    TermNode *uzel;

    if (p == NULL || stepen < 0) {
        return STATUS_INVALID_ARGUMENT;
    }
    if (koef == 0LL) {
        return STATUS_OK;
    }

    tek = p->golova;
    while (tek != NULL && tek->stepen > stepen) {
        pred = tek;
        tek = tek->sled;
    }

    if (tek != NULL && tek->stepen == stepen) {
        tek->koef += koef;
        if (tek->koef == 0LL) {
            if (pred == NULL) {
                p->golova = tek->sled;
            } else {
                pred->sled = tek->sled;
            }
            free(tek);
        }
        return STATUS_OK;
    }

    uzel = (TermNode *)malloc(sizeof(TermNode));
    if (uzel == NULL) {
        return STATUS_MEMORY_ERROR;
    }
    uzel->koef = koef;
    uzel->stepen = stepen;
    uzel->sled = tek;

    if (pred == NULL) {
        p->golova = uzel;
    } else {
        pred->sled = uzel;
    }
    return STATUS_OK;
}

static StatusCode poly_clone(const Polynomial *ist, Polynomial *naznach)
{
    const TermNode *tek;
    poly_init(naznach);

    for (tek = ist->golova; tek != NULL; tek = tek->sled) {
        StatusCode st = poly_add_term(naznach, tek->koef, tek->stepen);
        if (st != STATUS_OK) {
            poly_free(naznach);
            return st;
        }
    }
    return STATUS_OK;
}

static StatusCode poly_add(const Polynomial *a, const Polynomial *b, Polynomial *rez)
{
    const TermNode *tek;
    Polynomial vrem;
    StatusCode st;

    poly_init(&vrem);
    st = poly_clone(a, &vrem);
    if (st != STATUS_OK) {
        return st;
    }
    for (tek = b->golova; tek != NULL; tek = tek->sled) {
        st = poly_add_term(&vrem, tek->koef, tek->stepen);
        if (st != STATUS_OK) {
            poly_free(&vrem);
            return st;
        }
    }
    poly_free(rez);
    *rez = vrem;
    return STATUS_OK;
}

static StatusCode poly_sub(const Polynomial *a, const Polynomial *b, Polynomial *rez)
{
    const TermNode *tek;
    Polynomial vrem;
    StatusCode st;

    poly_init(&vrem);
    st = poly_clone(a, &vrem);
    if (st != STATUS_OK) {
        return st;
    }
    for (tek = b->golova; tek != NULL; tek = tek->sled) {
        st = poly_add_term(&vrem, -tek->koef, tek->stepen);
        if (st != STATUS_OK) {
            poly_free(&vrem);
            return st;
        }
    }
    poly_free(rez);
    *rez = vrem;
    return STATUS_OK;
}

static StatusCode poly_mult(const Polynomial *a, const Polynomial *b, Polynomial *rez)
{
    const TermNode *ca;
    const TermNode *cb;
    Polynomial vrem;

    poly_init(&vrem);
    for (ca = a->golova; ca != NULL; ca = ca->sled) {
        for (cb = b->golova; cb != NULL; cb = cb->sled) {
            StatusCode st = poly_add_term(&vrem,
                                          ca->koef * cb->koef,
                                          ca->stepen + cb->stepen);
            if (st != STATUS_OK) {
                poly_free(&vrem);
                return st;
            }
        }
    }
    poly_free(rez);
    *rez = vrem;
    return STATUS_OK;
}

/* деление многочленов с остатком */
static StatusCode poly_div_mod(const Polynomial *a,
                               const Polynomial *b,
                               Polynomial *chastnoe_out,
                               Polynomial *ostatok_out)
{
    Polynomial q;
    Polynomial r;
    StatusCode st;

    if (b->golova == NULL) {
        return STATUS_DIVISION_BY_ZERO;
    }

    poly_init(&q);
    poly_init(&r);
    st = poly_clone(a, &r);
    if (st != STATUS_OK) {
        return st;
    }

    while (r.golova != NULL && r.golova->stepen >= b->golova->stepen) {
        long long star_r = r.golova->koef;
        long long star_b = b->golova->koef;
        long long koef_chastn = star_r / star_b;
        int step_chastn = r.golova->stepen - b->golova->stepen;
        const TermNode *cb;

        /* остановка если старший коэффициент не делится нацело */
        if (koef_chastn == 0LL) {
            break;
        }

        st = poly_add_term(&q, koef_chastn, step_chastn);
        if (st != STATUS_OK) {
            poly_free(&q);
            poly_free(&r);
            return st;
        }

        for (cb = b->golova; cb != NULL; cb = cb->sled) {
            st = poly_add_term(&r, -koef_chastn * cb->koef, cb->stepen + step_chastn);
            if (st != STATUS_OK) {
                poly_free(&q);
                poly_free(&r);
                return st;
            }
        }
    }

    if (chastnoe_out != NULL) {
        poly_free(chastnoe_out);
        *chastnoe_out = q;
    } else {
        poly_free(&q);
    }

    if (ostatok_out != NULL) {
        poly_free(ostatok_out);
        *ostatok_out = r;
    } else {
        poly_free(&r);
    }

    return STATUS_OK;
}

static StatusCode poly_eval(const Polynomial *p, long long x, long long *znach_out)
{
    const TermNode *tek;
    long long obsh = 0LL;

    if (p == NULL || znach_out == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }

    for (tek = p->golova; tek != NULL; tek = tek->sled) {
        long long pokaz = 1LL;
        int i;
        for (i = 0; i < tek->stepen; ++i) {
            pokaz *= x;
        }
        obsh += tek->koef * pokaz;
    }

    *znach_out = obsh;
    return STATUS_OK;
}

static StatusCode poly_diff(const Polynomial *p, Polynomial *rez)
{
    const TermNode *tek;
    Polynomial vrem;

    poly_init(&vrem);
    for (tek = p->golova; tek != NULL; tek = tek->sled) {
        if (tek->stepen > 0) {
            StatusCode st = poly_add_term(&vrem,
                                          tek->koef * (long long)tek->stepen,
                                          tek->stepen - 1);
            if (st != STATUS_OK) {
                poly_free(&vrem);
                return st;
            }
        }
    }
    poly_free(rez);
    *rez = vrem;
    return STATUS_OK;
}

/* композиция многочленов */
static StatusCode poly_cmps(const Polynomial *p, const Polynomial *q, Polynomial *rez)
{
    const TermNode *tek;
    Polynomial obsh;

    poly_init(&obsh);
    for (tek = p->golova; tek != NULL; tek = tek->sled) {
        Polynomial pokaz;
        Polynomial koef_mnog;
        int i;

        poly_init(&pokaz);
        poly_init(&koef_mnog);
        poly_add_term(&pokaz, 1LL, 0);

        for (i = 0; i < tek->stepen; ++i) {
            if (poly_mult(&pokaz, q, &pokaz) != STATUS_OK) {
                poly_free(&pokaz);
                poly_free(&obsh);
                return STATUS_MEMORY_ERROR;
            }
        }

        poly_add_term(&koef_mnog, tek->koef, 0);
        poly_mult(&pokaz, &koef_mnog, &pokaz);
        poly_add(&obsh, &pokaz, &obsh);

        poly_free(&pokaz);
        poly_free(&koef_mnog);
    }

    poly_free(rez);
    *rez = obsh;
    return STATUS_OK;
}

/* парсинг строки многочлена типа 2x^2-x+2 */
static StatusCode parse_polynomial_str(const char *vhod_str, Polynomial *vyhod)
{
    const char *p = vhod_str;
    Polynomial vrem;

    if (vhod_str == NULL || vyhod == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }

    while (isspace((unsigned char)*p)) {
        ++p;
    }
    if (*p == '\0') {
        return STATUS_SYNTAX_ERROR;
    }

    poly_init(&vrem);

    while (*p != '\0') {
        int znak = 1;
        long long koef = 0LL;
        int est_koef = 0;
        int stepen = 0;

        while (isspace((unsigned char)*p)) {
            ++p;
        }
        if (*p == '\0') {
            break;
        }

        if (*p == '+') {
            znak = 1;
            ++p;
        } else if (*p == '-') {
            znak = -1;
            ++p;
        }

        while (isspace((unsigned char)*p)) {
            ++p;
        }

        while (isdigit((unsigned char)*p)) {
            koef = koef * 10LL + (*p - '0');
            est_koef = 1;
            ++p;
        }

        if (*p == 'x' || *p == 'X') {
            if (!est_koef) {
                koef = 1LL;
            }
            ++p;
            if (*p == '^') {
                ++p;
                if (!isdigit((unsigned char)*p)) {
                    poly_free(&vrem);
                    return STATUS_SYNTAX_ERROR;
                }
                stepen = 0;
                while (isdigit((unsigned char)*p)) {
                    stepen = stepen * 10 + (*p - '0');
                    ++p;
                }
            } else {
                stepen = 1;
            }
        } else {
            if (!est_koef) {
                poly_free(&vrem);
                return STATUS_SYNTAX_ERROR;
            }
            stepen = 0;
        }

        if (poly_add_term(&vrem, (long long)znak * koef, stepen) != STATUS_OK) {
            poly_free(&vrem);
            return STATUS_MEMORY_ERROR;
        }
    }

    poly_free(vyhod);
    *vyhod = vrem;
    return STATUS_OK;
}

static void poly_print(const Polynomial *p)
{
    const TermNode *tek = p->golova;
    int perv = 1;

    if (tek == NULL) {
        puts("0");
        return;
    }

    while (tek != NULL) {
        long long c = tek->koef;
        unsigned long long mod_koef = (c < 0LL) ? (unsigned long long)(-c) : (unsigned long long)c;

        if (!perv) {
            printf("%s", c >= 0LL ? " + " : " - ");
        } else if (c < 0LL) {
            putchar('-');
        }

        if (tek->stepen == 0 || mod_koef != 1ULL) {
            printf("%llu", mod_koef);
        }
        if (tek->stepen >= 1) {
            putchar('x');
            if (tek->stepen > 1) {
                printf("^%d", tek->stepen);
            }
        }

        perv = 0;
        tek = tek->sled;
    }
    putchar('\n');
}

/* чтение файла и вырезка комментариев */
static StatusCode read_clean_script(const char *put_f, char **chist_out)
{
    FILE *file;
    char *buf = NULL;
    size_t dlina = 0U;
    size_t emkost = 256U;
    int ch;
    int v_str_kom = 0;
    int v_blok_kom = 0;

    file = fopen(put_f, "r");
    if (file == NULL) {
        return STATUS_FILE_ERROR;
    }

    buf = (char *)malloc(emkost);
    if (buf == NULL) {
        fclose(file);
        return STATUS_MEMORY_ERROR;
    }

    while ((ch = fgetc(file)) != EOF) {
        if (v_str_kom) {
            if (ch == '\n') {
                v_str_kom = 0;
            }
            continue;
        }
        if (v_blok_kom) {
            if (ch == '[') {
                /* вложенные многострочные комменты запрещены */
                free(buf);
                fclose(file);
                return STATUS_SYNTAX_ERROR;
            }
            if (ch == ']') {
                v_blok_kom = 0;
            }
            continue;
        }

        if (ch == '%') {
            v_str_kom = 1;
            continue;
        }
        if (ch == '[') {
            v_blok_kom = 1;
            continue;
        }
        if (ch == ']') {
            free(buf);
            fclose(file);
            return STATUS_SYNTAX_ERROR;
        }

        if (!isspace((unsigned char)ch)) {
            if (dlina + 1U >= emkost) {
                char *nov_mas;
                emkost *= 2U;
                nov_mas = (char *)realloc(buf, emkost);
                if (nov_mas == NULL) {
                    free(buf);
                    fclose(file);
                    return STATUS_MEMORY_ERROR;
                }
                buf = nov_mas;
            }
            buf[dlina++] = (char)ch;
        }
    }

    fclose(file);
    if (v_blok_kom) {
        free(buf);
        return STATUS_SYNTAX_ERROR;
    }
    buf[dlina] = '\0';
    *chist_out = buf;
    return STATUS_OK;
}

/* выполнение одной инструкции */
static StatusCode execute_instruction(const char *instr, Polynomial *nakop)
{
    const char *otkr_skob = strchr(instr, '(');
    const char *zakr_skob = strrchr(instr, ')');
    char op[16];
    char arg_buf[512];
    char *zapyataya;
    size_t dlina_op;
    size_t arg_dlina;
    Polynomial p1, p2, rezult;
    StatusCode st = STATUS_OK;

    if (otkr_skob == NULL || zakr_skob == NULL || zakr_skob < otkr_skob || zakr_skob[1] != '\0') {
        return STATUS_SYNTAX_ERROR;
    }

    dlina_op = (size_t)(otkr_skob - instr);
    arg_dlina = (size_t)(zakr_skob - (otkr_skob + 1));
    if (dlina_op == 0U || dlina_op >= sizeof(op) || arg_dlina >= sizeof(arg_buf)) {
        return STATUS_SYNTAX_ERROR;
    }

    memcpy(op, instr, dlina_op);
    op[dlina_op] = '\0';
    memcpy(arg_buf, otkr_skob + 1, arg_dlina);
    arg_buf[arg_dlina] = '\0';

    poly_init(&p1);
    poly_init(&p2);
    poly_init(&rezult);

    zapyataya = strchr(arg_buf, ',');

    if (strcmp(op, "Eval") == 0) {
        long long znach_x = 0LL;
        long long rez_znach = 0LL;
        char *ukaz_end = NULL;
        const char *x_str = (zapyataya != NULL) ? (zapyataya + 1) : arg_buf;

        if (zapyataya != NULL) {
            *zapyataya = '\0';
            st = parse_polynomial_str(arg_buf, &p1);
        } else {
            st = poly_clone(nakop, &p1);
        }
        if (st != STATUS_OK) {
            poly_free(&p1);
            return st;
        }

        znach_x = strtoll(x_str, &ukaz_end, 10);
        if (ukaz_end == x_str || *ukaz_end != '\0') {
            poly_free(&p1);
            return STATUS_SYNTAX_ERROR;
        }

        st = poly_eval(&p1, znach_x, &rez_znach);
        poly_free(&p1);
        if (st == STATUS_OK) {
            printf("Eval(%lld) = %lld\n", znach_x, rez_znach);
        }
        return st;
    }

    if (strcmp(op, "Diff") == 0) {
        if (zapyataya != NULL) {
            return STATUS_SYNTAX_ERROR;
        }
        if (arg_buf[0] == '\0') {
            st = poly_clone(nakop, &p1);
        } else {
            st = parse_polynomial_str(arg_buf, &p1);
        }
        if (st == STATUS_OK) {
            st = poly_diff(&p1, &rezult);
        }
        poly_free(&p1);
        if (st == STATUS_OK) {
            poly_free(nakop);
            *nakop = rezult;
            printf("%s -> ", instr);
            poly_print(nakop);
        }
        return st;
    }

    /* без запятой первый аргумент из сумматора */
    if (zapyataya != NULL) {
        *zapyataya = '\0';
        st = parse_polynomial_str(arg_buf, &p1);
        if (st == STATUS_OK) {
            st = parse_polynomial_str(zapyataya + 1, &p2);
        }
    } else {
        st = poly_clone(nakop, &p1);
        if (st == STATUS_OK) {
            st = parse_polynomial_str(arg_buf, &p2);
        }
    }

    if (st != STATUS_OK) {
        poly_free(&p1);
        poly_free(&p2);
        return st;
    }

    if (strcmp(op, "Add") == 0) {
        st = poly_add(&p1, &p2, &rezult);
    } else if (strcmp(op, "Sub") == 0) {
        st = poly_sub(&p1, &p2, &rezult);
    } else if (strcmp(op, "Mult") == 0) {
        st = poly_mult(&p1, &p2, &rezult);
    } else if (strcmp(op, "Div") == 0) {
        st = poly_div_mod(&p1, &p2, &rezult, NULL);
    } else if (strcmp(op, "Mod") == 0) {
        st = poly_div_mod(&p1, &p2, NULL, &rezult);
    } else if (strcmp(op, "Cmps") == 0) {
        st = poly_cmps(&p1, &p2, &rezult);
    } else {
        st = STATUS_SYNTAX_ERROR;
    }

    poly_free(&p1);
    poly_free(&p2);

    if (st == STATUS_OK) {
        poly_free(nakop);
        *nakop = rezult;
        printf("%s -> ", op);
        poly_print(nakop);
    } else {
        poly_free(&rezult);
    }
    return st;
}

int main(int argc, char **argv)
{
    char *chist_skript = NULL;
    char *tok;
    Polynomial sumator;
    StatusCode st_kod;

    if (argc != 2) {
        fprintf(stderr, "Использование: %s <файл_инструкций>\n", argv[0]);
        return 1;
    }

    st_kod = read_clean_script(argv[1], &chist_skript);
    if (st_kod != STATUS_OK) {
        fprintf(stderr, "Ошибка чтения/парсинга комментариев (код %d).\n", (int)st_kod);
        return 1;
    }

    poly_init(&sumator);

    tok = strtok(chist_skript, ";");
    while (tok != NULL) {
        if (tok[0] != '\0') {
            st_kod = execute_instruction(tok, &sumator);
            if (st_kod != STATUS_OK) {
                fprintf(stderr, "Ошибка выполнения инструкции «%s» (код %d).\n",
                        tok, (int)st_kod);
                poly_free(&sumator);
                free(chist_skript);
                return 1;
            }
        }
        tok = strtok(NULL, ";");
    }

    poly_free(&sumator);
    free(chist_skript);
    return 0;
}
