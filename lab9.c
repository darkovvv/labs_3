#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
    STATUS_OK = 0,
    STATUS_INVALID_ARGUMENT,
    STATUS_FILE_ERROR,
    STATUS_MEMORY_ERROR
} StatusCode;

typedef struct BSTNode {
    char *slovo;
    unsigned long long kolvo;
    struct BSTNode *lev;
    struct BSTNode *prav;
} BSTNode;

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

static char *dup_word(const char *vhod_str)
{
    size_t dlina = strlen(vhod_str);
    char *kopiya = (char *)malloc(dlina + 1U);
    if (kopiya != NULL) {
        memcpy(kopiya, vhod_str, dlina + 1U);
    }
    return kopiya;
}

static BSTNode *bst_create_node(const char *slovo, unsigned long long kolvo)
{
    BSTNode *uzel = (BSTNode *)malloc(sizeof(BSTNode));
    if (uzel == NULL) {
        return NULL;
    }
    uzel->slovo = dup_word(slovo);
    if (uzel->slovo == NULL) {
        free(uzel);
        return NULL;
    }
    uzel->kolvo = kolvo;
    uzel->lev = NULL;
    uzel->prav = NULL;
    return uzel;
}

static void bst_free(BSTNode *koren)
{
    if (koren == NULL) {
        return;
    }
    bst_free(koren->lev);
    bst_free(koren->prav);
    free(koren->slovo);
    free(koren);
}

/* вставка слова или инкремент счетчика */
static StatusCode bst_insert(BSTNode **koren, const char *slovo, unsigned long long dob_kolvo)
{
    int rez_srav;

    if (koren == NULL || slovo == NULL || slovo[0] == '\0') {
        return STATUS_INVALID_ARGUMENT;
    }

    if (*koren == NULL) {
        *koren = bst_create_node(slovo, dob_kolvo);
        return (*koren != NULL) ? STATUS_OK : STATUS_MEMORY_ERROR;
    }

    rez_srav = strcmp(slovo, (*koren)->slovo);
    if (rez_srav == 0) {
        (*koren)->kolvo += dob_kolvo;
        return STATUS_OK;
    } else if (rez_srav < 0) {
        return bst_insert(&(*koren)->lev, slovo, dob_kolvo);
    } else {
        return bst_insert(&(*koren)->prav, slovo, dob_kolvo);
    }
}

static StatusCode bst_find_count(const BSTNode *koren,
                                 const char *slovo,
                                 unsigned long long *kolvo_out)
{
    const BSTNode *tek = koren;

    if (slovo == NULL || kolvo_out == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }

    while (tek != NULL) {
        int rez_srav = strcmp(slovo, tek->slovo);
        if (rez_srav == 0) {
            *kolvo_out = tek->kolvo;
            return STATUS_OK;
        }
        tek = (rez_srav < 0) ? tek->lev : tek->prav;
    }

    *kolvo_out = 0ULL;
    return STATUS_OK;
}

/* глубина дерева */
static StatusCode bst_depth(const BSTNode *koren, size_t *glubina_out)
{
    size_t lev_gl = 0U;
    size_t prav_gl = 0U;

    if (glubina_out == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    if (koren == NULL) {
        *glubina_out = 0U;
        return STATUS_OK;
    }

    bst_depth(koren->lev, &lev_gl);
    bst_depth(koren->prav, &prav_gl);
    *glubina_out = 1U + (lev_gl > prav_gl ? lev_gl : prav_gl);
    return STATUS_OK;
}

/* поиск самого длинного и короткого слова */
static void bst_extremes_rec(const BSTNode *uzel,
                             const char **korot_out,
                             const char **dlin_out)
{
    size_t dlina;
    if (uzel == NULL) {
        return;
    }
    dlina = strlen(uzel->slovo);
    if (*korot_out == NULL || dlina < strlen(*korot_out)) {
        *korot_out = uzel->slovo;
    }
    if (*dlin_out == NULL || dlina > strlen(*dlin_out)) {
        *dlin_out = uzel->slovo;
    }
    bst_extremes_rec(uzel->lev, korot_out, dlin_out);
    bst_extremes_rec(uzel->prav, korot_out, dlin_out);
}

static StatusCode bst_find_shortest_and_longest(const BSTNode *koren,
                                                const char **korot_out,
                                                const char **dlin_out)
{
    if (koren == NULL || korot_out == NULL || dlin_out == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    *korot_out = NULL;
    *dlin_out = NULL;
    bst_extremes_rec(koren, korot_out, dlin_out);
    return STATUS_OK;
}

/* сбор узлов в массив для топ-n */
static size_t bst_count_nodes(const BSTNode *koren)
{
    if (koren == NULL) {
        return 0U;
    }
    return 1U + bst_count_nodes(koren->lev) + bst_count_nodes(koren->prav);
}

static void bst_collect_nodes(const BSTNode *koren, const BSTNode **massiv, size_t *ind)
{
    if (koren == NULL) {
        return;
    }
    bst_collect_nodes(koren->lev, massiv, ind);
    massiv[(*ind)++] = koren;
    bst_collect_nodes(koren->prav, massiv, ind);
}

static int cmp_nodes_by_freq_desc(const void *a, const void *b)
{
    const BSTNode * const *lev = (const BSTNode * const *)a;
    const BSTNode * const *prav = (const BSTNode * const *)b;

    if ((*lev)->kolvo != (*prav)->kolvo) {
        return ((*prav)->kolvo > (*lev)->kolvo) - ((*prav)->kolvo < (*lev)->kolvo);
    }
    return strcmp((*lev)->slovo, (*prav)->slovo);
}

/* дамп дерева в преордере */
static int save_preorder_rec(FILE *file, const BSTNode *uzel)
{
    if (uzel == NULL) {
        return fprintf(file, "#\n") >= 0;
    }
    if (fprintf(file, "%llu %s\n", uzel->kolvo, uzel->slovo) < 0) {
        return 0;
    }
    return save_preorder_rec(file, uzel->lev) &&
           save_preorder_rec(file, uzel->prav);
}

static StatusCode bst_save_to_file(const BSTNode *koren, const char *put_f)
{
    FILE *file;
    int ok;

    if (put_f == NULL || put_f[0] == '\0') {
        return STATUS_INVALID_ARGUMENT;
    }

    file = fopen(put_f, "w");
    if (file == NULL) {
        return STATUS_FILE_ERROR;
    }

    ok = save_preorder_rec(file, koren);
    fclose(file);
    return ok ? STATUS_OK : STATUS_FILE_ERROR;
}

/* восстановление дерева из преордера */
static StatusCode load_preorder_rec(FILE *file, BSTNode **uzel_out)
{
    char stroka[1024];
    unsigned long long kolvo;
    char slovo_buf[1024];
    BSTNode *uzel;
    StatusCode st;

    if (fgets(stroka, sizeof(stroka), file) == NULL) {
        return STATUS_FILE_ERROR;
    }
    stroka[strcspn(stroka, "\r\n")] = '\0';

    if (strcmp(stroka, "#") == 0) {
        *uzel_out = NULL;
        return STATUS_OK;
    }

    if (sscanf(stroka, "%llu %1023s", &kolvo, slovo_buf) != 2) {
        return STATUS_FILE_ERROR;
    }

    uzel = bst_create_node(slovo_buf, kolvo);
    if (uzel == NULL) {
        return STATUS_MEMORY_ERROR;
    }

    st = load_preorder_rec(file, &uzel->lev);
    if (st != STATUS_OK) {
        bst_free(uzel);
        return st;
    }
    st = load_preorder_rec(file, &uzel->prav);
    if (st != STATUS_OK) {
        bst_free(uzel);
        return st;
    }

    *uzel_out = uzel;
    return STATUS_OK;
}

static StatusCode bst_load_from_file(const char *put_f, BSTNode **koren_out)
{
    FILE *file;
    StatusCode st;

    if (put_f == NULL || koren_out == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }

    file = fopen(put_f, "r");
    if (file == NULL) {
        return STATUS_FILE_ERROR;
    }

    *koren_out = NULL;
    st = load_preorder_rec(file, koren_out);
    fclose(file);
    return st;
}

static int bst_equals_structure(const BSTNode *a, const BSTNode *b)
{
    if (a == NULL && b == NULL) {
        return 1;
    }
    if (a == NULL || b == NULL) {
        return 0;
    }
    return a->kolvo == b->kolvo &&
           strcmp(a->slovo, b->slovo) == 0 &&
           bst_equals_structure(a->lev, b->lev) &&
           bst_equals_structure(a->prav, b->prav);
}

static int is_separator(char ch, int argc, char **argv)
{
    int i;
    if (ch == '\n' || ch == '\r' || ch == '\t' || ch == ' ') {
        return 1;
    }
    for (i = 2; i < argc; ++i) {
        size_t j;
        for (j = 0; argv[i][j] != '\0'; ++j) {
            if (ch == argv[i][j]) {
                return 1;
            }
        }
    }
    return 0;
}

/* разбор файла на слова по сепараторам */
static StatusCode parse_file_to_bst(const char *put_f,
                                    int argc,
                                    char **argv,
                                    BSTNode **koren_out)
{
    FILE *file;
    size_t emkost = 64U;
    size_t dlina = 0U;
    char *slovo = NULL;
    int ch;

    file = fopen(put_f, "r");
    if (file == NULL) {
        return STATUS_FILE_ERROR;
    }

    slovo = (char *)malloc(emkost);
    if (slovo == NULL) {
        fclose(file);
        return STATUS_MEMORY_ERROR;
    }

    while ((ch = fgetc(file)) != EOF) {
        if (is_separator((char)ch, argc, argv)) {
            if (dlina > 0U) {
                slovo[dlina] = '\0';
                if (bst_insert(koren_out, slovo, 1ULL) != STATUS_OK) {
                    free(slovo);
                    fclose(file);
                    return STATUS_MEMORY_ERROR;
                }
                dlina = 0U;
            }
        } else {
            if (dlina + 1U >= emkost) {
                char *nov_mas;
                emkost *= 2U;
                nov_mas = (char *)realloc(slovo, emkost);
                if (nov_mas == NULL) {
                    free(slovo);
                    fclose(file);
                    return STATUS_MEMORY_ERROR;
                }
                slovo = nov_mas;
            }
            slovo[dlina++] = (char)ch;
        }
    }

    if (dlina > 0U) {
        slovo[dlina] = '\0';
        if (bst_insert(koren_out, slovo, 1ULL) != STATUS_OK) {
            free(slovo);
            fclose(file);
            return STATUS_MEMORY_ERROR;
        }
    }

    free(slovo);
    fclose(file);
    return STATUS_OK;
}

static void bst_print_visual(const BSTNode *uzel, size_t otstup)
{
    size_t i;
    if (uzel == NULL) {
        return;
    }
    bst_print_visual(uzel->prav, otstup + 1U);
    for (i = 0U; i < otstup; ++i) {
        printf("    ");
    }
    printf("%s (%llu)\n", uzel->slovo, uzel->kolvo);
    bst_print_visual(uzel->lev, otstup + 1U);
}

int main(int argc, char **argv)
{
    BSTNode *koren = NULL;
    char stroka[256];
    size_t glubina = 0U;

    if (argc < 2) {
        fprintf(stderr, "Использование: %s <файл_текста> [сепаратор1 сепаратор2 ...]\n", argv[0]);
        return 1;
    }

    if (parse_file_to_bst(argv[1], argc, argv, &koren) != STATUS_OK) {
        fprintf(stderr, "Ошибка чтения файла.\n");
        bst_free(koren);
        return 1;
    }

    bst_depth(koren, &glubina);
    printf("Дерево построено. Глубина дерева (п. B): %zu\n", glubina);

    /* интерактивный диалог в сишке */
    for (;;) {
        puts("\n=== Меню статистики текста (BST) ===");
        puts("1 - Показать дерево слов и частот");
        puts("2 - Найти частоту заданного слова");
        puts("3 - Вывести первые n наиболее частых слов");
        puts("4 - Найти самое короткое и самое длинное слово");
        puts("5 - Показать глубину дерева (п. B)");
        puts("6 - Сохранить дерево в файл и восстановить из файла (п. C)");
        puts("0 - Выход");
        printf("Выбор: ");

        if (fgets(stroka, sizeof(stroka), stdin) == NULL) {
            break;
        }
        stroka[strcspn(stroka, "\r\n")] = '\0';

        if (strcmp(stroka, "0") == 0) {
            break;
        } else if (strcmp(stroka, "1") == 0) {
            if (koren == NULL) {
                puts("Дерево пусто.");
            } else {
                bst_print_visual(koren, 0U);
            }
        } else if (strcmp(stroka, "2") == 0) {
            unsigned long long schet = 0ULL;
            printf("Введите слово: ");
            if (fgets(stroka, sizeof(stroka), stdin) == NULL) break;
            stroka[strcspn(stroka, "\r\n")] = '\0';
            bst_find_count(koren, stroka, &schet);
            printf("Слово «%s» встретилось %llu раз(а).\n", stroka, schet);
        } else if (strcmp(stroka, "3") == 0) {
            size_t n = 0U;
            size_t obsh = bst_count_nodes(koren);
            const BSTNode **massiv = NULL;
            size_t ind = 0U;
            size_t i;

            printf("Введите n: ");
            if (fgets(stroka, sizeof(stroka), stdin) == NULL ||
                sscanf(stroka, "%zu", &n) != 1 || n == 0U) {
                puts("Некорректное число n.");
                continue;
            }
            if (obsh == 0U) {
                puts("Дерево пусто.");
                continue;
            }
            massiv = (const BSTNode **)malloc(obsh * sizeof(const BSTNode *));
            if (massiv == NULL) {
                puts("Ошибка памяти.");
                continue;
            }
            bst_collect_nodes(koren, massiv, &ind);
            qsort((void *)massiv, obsh, sizeof(const BSTNode *), cmp_nodes_by_freq_desc);
            if (n > obsh) {
                n = obsh;
            }
            for (i = 0U; i < n; ++i) {
                printf("  %zu. %s — %llu\n", i + 1U, massiv[i]->slovo, massiv[i]->kolvo);
            }
            free((void *)massiv);
        } else if (strcmp(stroka, "4") == 0) {
            const char *samoe_korot = NULL;
            const char *samoe_dlin = NULL;
            if (bst_find_shortest_and_longest(koren, &samoe_korot, &samoe_dlin) == STATUS_OK) {
                printf("Самое короткое слово: «%s» (длина %zu)\n", samoe_korot, strlen(samoe_korot));
                printf("Самое длинное слово:  «%s» (длина %zu)\n", samoe_dlin, strlen(samoe_dlin));
            } else {
                puts("Дерево пусто.");
            }
        } else if (strcmp(stroka, "5") == 0) {
            bst_depth(koren, &glubina);
            printf("Глубина дерева: %zu\n", glubina);
        } else if (strcmp(stroka, "6") == 0) {
            BSTNode *vosstan = NULL;
            printf("Введите имя файла для сохранения дерева: ");
            if (fgets(stroka, sizeof(stroka), stdin) == NULL) break;
            stroka[strcspn(stroka, "\r\n")] = '\0';
            if (are_same_files(argv[1], stroka)) {
                puts("Файл дампа не должен совпадать с входным файлом.");
                continue;
            }

            if (bst_save_to_file(koren, stroka) != STATUS_OK) {
                puts("Ошибка сохранения дерева.");
                continue;
            }
            if (bst_load_from_file(stroka, &vosstan) != STATUS_OK) {
                puts("Ошибка восстановления дерева.");
                continue;
            }
            printf("Дерево восстановлено. Совпадение структуры и вида: %s\n",
                   bst_equals_structure(koren, vosstan) ? "ДА (100%)" : "НЕТ");
            bst_free(koren);
            koren = vosstan;
        } else {
            puts("Неизвестная команда.");
        }
    }

    bst_free(koren);
    return 0;
}
