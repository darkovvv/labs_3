#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
    STATUS_OK = 0,
    STATUS_INVALID_ARGUMENT,
    STATUS_SYNTAX_ERROR,
    STATUS_FILE_ERROR,
    STATUS_MEMORY_ERROR
} StatusCode;

typedef struct TreeNode {
    char metka[64];
    struct TreeNode *perv_potomok;
    struct TreeNode *sled_brat;
} TreeNode;

/* проверка совпадения путей файлов */
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

static TreeNode *create_tree_node(const char *metka, size_t dlina)
{
    TreeNode *uzel;

    if (dlina == 0U || dlina >= sizeof(uzel->metka)) {
        return NULL;
    }
    uzel = (TreeNode *)malloc(sizeof(TreeNode));
    if (uzel == NULL) {
        return NULL;
    }
    memcpy(uzel->metka, metka, dlina);
    uzel->metka[dlina] = '\0';
    uzel->perv_potomok = NULL;
    uzel->sled_brat = NULL;
    return uzel;
}

static void free_general_tree(TreeNode *koren)
{
    if (koren == NULL) {
        return;
    }
    free_general_tree(koren->perv_potomok);
    free_general_tree(koren->sled_brat);
    free(koren);
}

static void skip_spaces(const char **p)
{
    while (**p != '\0' && isspace((unsigned char)**p)) {
        (*p)++;
    }
}

/* рекурсивный парсинг узла и списка детей */
static StatusCode parse_node_rec(const char **p, TreeNode **uzel_out)
{
    const char *nach;
    size_t dlina = 0U;
    TreeNode *uzel;

    skip_spaces(p);
    nach = *p;
    while (**p != '\0' && !isspace((unsigned char)**p) &&
           **p != '(' && **p != ')' && **p != ',') {
        (*p)++;
        dlina++;
    }
    if (dlina == 0U) {
        return STATUS_SYNTAX_ERROR;
    }

    uzel = create_tree_node(nach, dlina);
    if (uzel == NULL) {
        return STATUS_MEMORY_ERROR;
    }

    skip_spaces(p);
    if (**p == '(') {
        TreeNode *posl_potomok = NULL;
        (*p)++;

        for (;;) {
            TreeNode *potomok = NULL;
            StatusCode st = parse_node_rec(p, &potomok);
            if (st != STATUS_OK) {
                free_general_tree(uzel);
                return st;
            }

            if (posl_potomok == NULL) {
                uzel->perv_potomok = potomok;
            } else {
                posl_potomok->sled_brat = potomok;
            }
            posl_potomok = potomok;

            skip_spaces(p);
            if (**p == ',') {
                (*p)++;
                continue;
            } else if (**p == ')') {
                (*p)++;
                break;
            } else {
                free_general_tree(uzel);
                return STATUS_SYNTAX_ERROR;
            }
        }
    }

    *uzel_out = uzel;
    return STATUS_OK;
}

/* парсинг скобочного выражения */
static StatusCode parse_bracket_expression(const char *stroka, TreeNode **koren_out)
{
    const char *p = stroka;
    TreeNode *koren = NULL;
    StatusCode st;

    if (stroka == NULL || koren_out == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }

    st = parse_node_rec(&p, &koren);
    if (st != STATUS_OK) {
        return st;
    }

    skip_spaces(&p);
    if (*p != '\0') {
        free_general_tree(koren);
        return STATUS_SYNTAX_ERROR;
    }

    *koren_out = koren;
    return STATUS_OK;
}

/* печать дерева с ветками в файл */
static void print_tree_pretty(FILE *vyhod,
                              const TreeNode *uzel,
                              int glubina,
                              int poslednij,
                              const int *aktiv_urovni)
{
    int i;
    const TreeNode *potomok;
    int sled_urovni[128];

    if (uzel == NULL || glubina >= 128) {
        return;
    }

    for (i = 0; i < glubina - 1; ++i) {
        fprintf(vyhod, "%s", aktiv_urovni[i] ? "|   " : "    ");
    }
    if (glubina > 0) {
        fprintf(vyhod, "%s", poslednij ? "`-- " : "|-- ");
    }
    fprintf(vyhod, "%s\n", uzel->metka);

    for (i = 0; i < glubina; ++i) {
        sled_urovni[i] = aktiv_urovni[i];
    }
    if (glubina > 0) {
        sled_urovni[glubina - 1] = !poslednij;
    }

    potomok = uzel->perv_potomok;
    while (potomok != NULL) {
        print_tree_pretty(vyhod, potomok, glubina + 1, potomok->sled_brat == NULL, sled_urovni);
        potomok = potomok->sled_brat;
    }
}

int main(int argc, char **argv)
{
    FILE *f_vhod;
    FILE *f_vyhod;
    char stroka[2048];
    unsigned int nomer_vyr = 0U;

    /* валидация аргументов командной строки */
    if (argc != 3) {
        fprintf(stderr, "Использование: %s <входной_файл> <выходной_файл>\n", argv[0]);
        return 1;
    }
    if (are_same_files(argv[1], argv[2])) {
        fprintf(stderr, "Входной и выходной файлы не должны совпадать.\n");
        return 1;
    }

    f_vhod = fopen(argv[1], "r");
    if (f_vhod == NULL) {
        fprintf(stderr, "Не удалось открыть входной файл.\n");
        return 1;
    }
    f_vyhod = fopen(argv[2], "w");
    if (f_vyhod == NULL) {
        fprintf(stderr, "Не удалось открыть выходной файл.\n");
        fclose(f_vhod);
        return 1;
    }

    while (fgets(stroka, sizeof(stroka), f_vhod) != NULL) {
        TreeNode *koren = NULL;
        int aktiv_urovni[128] = {0};
        const char *proverka = stroka;
        StatusCode st;

        stroka[strcspn(stroka, "\r\n")] = '\0';
        skip_spaces(&proverka);
        if (*proverka == '\0') {
            continue;
        }

        nomer_vyr++;
        fprintf(f_vyhod, "=== Выражение #%u: %s ===\n", nomer_vyr, proverka);

        st = parse_bracket_expression(proverka, &koren);
        if (st != STATUS_OK) {
            fprintf(f_vyhod, "Ошибка разбора скобочного выражения (код %d).\n\n", (int)st);
            continue;
        }

        print_tree_pretty(f_vyhod, koren, 0, 1, aktiv_urovni);
        fputc('\n', f_vyhod);
        free_general_tree(koren);
    }

    fclose(f_vyhod);
    fclose(f_vhod);
    printf("Деревья успешно записаны в %s (обработано выражений: %u)\n", argv[2], nomer_vyr);
    return 0;
}
