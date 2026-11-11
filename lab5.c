#include <ctype.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define GRADES_COUNT 5

typedef enum {
    STATUS_OK = 0,
    STATUS_INVALID_ARGUMENT,
    STATUS_FILE_ERROR,
    STATUS_MEMORY_ERROR,
    STATUS_NOT_FOUND
} StatusCode;

typedef struct {
    unsigned int id;
    char *imya;
    char *familiya;
    char *gruppa;
    unsigned char *ocenki;
} Student;

/* проверка что файлы не совпадают */
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

static char *dup_string(const char *ist)
{
    size_t dlina;
    char *kopiya;

    if (ist == NULL) {
        return NULL;
    }
    dlina = strlen(ist);
    kopiya = (char *)malloc(dlina + 1U);
    if (kopiya == NULL) {
        return NULL;
    }
    memcpy(kopiya, ist, dlina + 1U);
    return kopiya;
}

/* проверка латиницы в строке */
static int is_latin_word(const char *vhod_str)
{
    size_t i;
    if (vhod_str == NULL || vhod_str[0] == '\0') {
        return 0;
    }
    for (i = 0; vhod_str[i] != '\0'; ++i) {
        if (!isalpha((unsigned char)vhod_str[i])) {
            return 0;
        }
    }
    return 1;
}

static void free_student(Student *student)
{
    if (student == NULL) {
        return;
    }
    free(student->imya);
    free(student->familiya);
    free(student->gruppa);
    free(student->ocenki);
    student->imya = NULL;
    student->familiya = NULL;
    student->gruppa = NULL;
    student->ocenki = NULL;
}

static void free_students(Student *studenty, size_t kolvo)
{
    size_t i;
    if (studenty == NULL) {
        return;
    }
    for (i = 0; i < kolvo; ++i) {
        free_student(&studenty[i]);
    }
    free(studenty);
}

static double student_average(const Student *student)
{
    int i;
    unsigned int obsh_sum = 0U;

    for (i = 0; i < GRADES_COUNT; ++i) {
        obsh_sum += student->ocenki[i];
    }
    return (double)obsh_sum / (double)GRADES_COUNT;
}

/* компараторы для qsort */
static int cmp_by_id(const void *a, const void *b)
{
    const Student *lev = (const Student *)a;
    const Student *prav = (const Student *)b;
    return (lev->id > prav->id) - (lev->id < prav->id);
}

static int cmp_by_surname(const void *a, const void *b)
{
    const Student *lev = (const Student *)a;
    const Student *prav = (const Student *)b;
    return strcmp(lev->familiya, prav->familiya);
}

static int cmp_by_name(const void *a, const void *b)
{
    const Student *lev = (const Student *)a;
    const Student *prav = (const Student *)b;
    return strcmp(lev->imya, prav->imya);
}

static int cmp_by_group(const void *a, const void *b)
{
    const Student *lev = (const Student *)a;
    const Student *prav = (const Student *)b;
    return strcmp(lev->gruppa, prav->gruppa);
}

/* чтение массива студентов из файла */
static StatusCode load_students(const char *put_f,
                                Student **studenty_out,
                                size_t *kolvo_out)
{
    FILE *file;
    Student *massiv = NULL;
    size_t kolvo = 0U;
    size_t emkost = 0U;

    if (put_f == NULL || studenty_out == NULL || kolvo_out == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }

    file = fopen(put_f, "r");
    if (file == NULL) {
        return STATUS_FILE_ERROR;
    }

    for (;;) {
        unsigned int id;
        char imya_buf[128];
        char fam_buf[128];
        char gruppa_buf[128];
        unsigned int g[GRADES_COUNT];
        int prochit_polei;
        int i;
        Student st;

        prochit_polei = fscanf(file, "%u %127s %127s %127s %u %u %u %u %u",
                            &id, imya_buf, fam_buf, gruppa_buf,
                            &g[0], &g[1], &g[2], &g[3], &g[4]);
        if (prochit_polei == EOF) {
            break;
        }
        if (prochit_polei != 9 || !is_latin_word(imya_buf) ||
            !is_latin_word(fam_buf) || gruppa_buf[0] == '\0') {
            free_students(massiv, kolvo);
            fclose(file);
            return STATUS_INVALID_ARGUMENT;
        }
        for (i = 0; i < GRADES_COUNT; ++i) {
            if (g[i] > 100U) {
                free_students(massiv, kolvo);
                fclose(file);
                return STATUS_INVALID_ARGUMENT;
            }
        }

        if (kolvo >= emkost) {
            size_t nov_emkost = emkost == 0U ? 8U : emkost * 2U;
            Student *nov_mas = (Student *)realloc(massiv, nov_emkost * sizeof(Student));
            if (nov_mas == NULL) {
                free_students(massiv, kolvo);
                fclose(file);
                return STATUS_MEMORY_ERROR;
            }
            massiv = nov_mas;
            emkost = nov_emkost;
        }

        st.id = id;
        st.imya = dup_string(imya_buf);
        st.familiya = dup_string(fam_buf);
        st.gruppa = dup_string(gruppa_buf);
        st.ocenki = (unsigned char *)malloc(GRADES_COUNT * sizeof(unsigned char));

        if (st.imya == NULL || st.familiya == NULL ||
            st.gruppa == NULL || st.ocenki == NULL) {
            free_student(&st);
            free_students(massiv, kolvo);
            fclose(file);
            return STATUS_MEMORY_ERROR;
        }
        for (i = 0; i < GRADES_COUNT; ++i) {
            st.ocenki[i] = (unsigned char)g[i];
        }

        massiv[kolvo++] = st;
    }

    fclose(file);
    *studenty_out = massiv;
    *kolvo_out = kolvo;
    return STATUS_OK;
}

/* запись найденного студента в трассировку */
static StatusCode trace_student_by_id(const Student *studenty,
                                      size_t kolvo,
                                      unsigned int iskomy_id,
                                      FILE *f_trass)
{
    size_t i;
    int nashli = 0;

    if (studenty == NULL || f_trass == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }

    for (i = 0; i < kolvo; ++i) {
        if (studenty[i].id == iskomy_id) {
            fprintf(f_trass,
                    "Студент ID=%u: %s %s, группа %s, средний балл: %.2f\n",
                    studenty[i].id,
                    studenty[i].familiya,
                    studenty[i].imya,
                    studenty[i].gruppa,
                    student_average(&studenty[i]));
            fflush(f_trass);
            nashli = 1;
        }
    }
    return nashli ? STATUS_OK : STATUS_NOT_FOUND;
}

/* запись студентов с баллом выше среднего */
static StatusCode trace_above_average(const Student *studenty,
                                      size_t kolvo,
                                      double tochnost_eps,
                                      FILE *f_trass)
{
    double obsh_srednee = 0.0;
    size_t i;

    if (studenty == NULL || f_trass == NULL || kolvo == 0U ||
        !isfinite(tochnost_eps) || tochnost_eps <= 0.0) {
        return STATUS_INVALID_ARGUMENT;
    }

    for (i = 0; i < kolvo; ++i) {
        obsh_srednee += student_average(&studenty[i]);
    }
    obsh_srednee /= (double)kolvo;

    fprintf(f_trass, "Студенты со средним баллом выше общего (%.2f):\n", obsh_srednee);
    for (i = 0; i < kolvo; ++i) {
        double srednee = student_average(&studenty[i]);
        if (srednee - obsh_srednee > tochnost_eps) {
            fprintf(f_trass, "  %s %s (средний балл: %.2f)\n",
                    studenty[i].familiya, studenty[i].imya, srednee);
        }
    }
    fflush(f_trass);
    return STATUS_OK;
}

static void print_student(const Student *s)
{
    printf("  ID: %4u | %-12s %-12s | Группа: %-10s | Оценки: %u %u %u %u %u (ср. %.2f)\n",
           s->id, s->familiya, s->imya, s->gruppa,
           s->ocenki[0], s->ocenki[1], s->ocenki[2], s->ocenki[3], s->ocenki[4],
           student_average(s));
}

int main(int argc, char **argv)
{
    Student *studenty = NULL;
    size_t kolvo = 0U;
    FILE *f_trass = NULL;
    StatusCode st_kod;
    char stroka[256];
    size_t i;

    if (argc != 3) {
        fprintf(stderr, "Использование: %s <файл_студентов> <трассировочный_файл>\n", argv[0]);
        return 1;
    }
    if (are_same_files(argv[1], argv[2])) {
        fprintf(stderr, "Входной и трассировочный файлы не должны совпадать.\n");
        return 1;
    }

    st_kod = load_students(argv[1], &studenty, &kolvo);
    if (st_kod != STATUS_OK) {
        fprintf(stderr, "Ошибка чтения файла студентов (код %d).\n", (int)st_kod);
        return 1;
    }

    f_trass = fopen(argv[2], "w");
    if (f_trass == NULL) {
        fprintf(stderr, "Не удалось открыть трассировочный файл.\n");
        free_students(studenty, kolvo);
        return 1;
    }

    /* интерактивный диалог */
    for (;;) {
        puts("=== Меню управления списком студентов ===");
        puts("1 - Показать всех студентов");
        puts("2 - Поиск по ID (и запись в трассировочный файл)");
        puts("3 - Поиск по фамилии");
        puts("4 - Поиск по имени");
        puts("5 - Поиск по группе");
        puts("6 - Сортировка по ID");
        puts("7 - Сортировка по фамилии");
        puts("8 - Сортировка по имени");
        puts("9 - Сортировка по группе");
        puts("10 - Вывести в трассировочный файл студентов с баллом выше среднего");
        puts("0 - Выход");
        printf("Выбор: ");

        if (fgets(stroka, sizeof(stroka), stdin) == NULL) {
            break;
        }
        stroka[strcspn(stroka, "\r\n")] = '\0';

        if (strcmp(stroka, "0") == 0) {
            break;
        } else if (strcmp(stroka, "1") == 0) {
            for (i = 0; i < kolvo; ++i) {
                print_student(&studenty[i]);
            }
        } else if (strcmp(stroka, "2") == 0) {
            unsigned int iskomy_id = 0U;
            printf("Введите ID: ");
            if (fgets(stroka, sizeof(stroka), stdin) == NULL ||
                sscanf(stroka, "%u", &iskomy_id) != 1) {
                puts("Некорректный ID.");
                continue;
            }
            for (i = 0; i < kolvo; ++i) {
                if (studenty[i].id == iskomy_id) {
                    print_student(&studenty[i]);
                }
            }
            if (trace_student_by_id(studenty, kolvo, iskomy_id, f_trass) == STATUS_OK) {
                puts("Данные записаны в трассировочный файл.");
            } else {
                puts("Студент с таким ID не найден.");
            }
        } else if (strcmp(stroka, "3") == 0 || strcmp(stroka, "4") == 0 || strcmp(stroka, "5") == 0) {
            int rezhim = stroka[0] - '0';
            int nashli = 0;
            printf("Введите строку для поиска: ");
            if (fgets(stroka, sizeof(stroka), stdin) == NULL) break;
            stroka[strcspn(stroka, "\r\n")] = '\0';

            for (i = 0; i < kolvo; ++i) {
                const char *pole = (rezhim == 3) ? studenty[i].familiya
                                  : (rezhim == 4) ? studenty[i].imya
                                                : studenty[i].gruppa;
                if (strcmp(pole, stroka) == 0) {
                    print_student(&studenty[i]);
                    nashli = 1;
                }
            }
            if (!nashli) {
                puts("Ничего не найдено.");
            }
        } else if (strcmp(stroka, "6") == 0) {
            qsort(studenty, kolvo, sizeof(Student), cmp_by_id);
            puts("Отсортировано по ID.");
        } else if (strcmp(stroka, "7") == 0) {
            qsort(studenty, kolvo, sizeof(Student), cmp_by_surname);
            puts("Отсортировано по фамилии.");
        } else if (strcmp(stroka, "8") == 0) {
            qsort(studenty, kolvo, sizeof(Student), cmp_by_name);
            puts("Отсортировано по имени.");
        } else if (strcmp(stroka, "9") == 0) {
            qsort(studenty, kolvo, sizeof(Student), cmp_by_group);
            puts("Отсортировано по группе.");
        } else if (strcmp(stroka, "10") == 0) {
            if (trace_above_average(studenty, kolvo, 1e-9, f_trass) == STATUS_OK) {
                puts("Список записан в трассировочный файл.");
            }
        } else {
            puts("Неизвестная команда.");
        }
        putchar('\n');
    }

    fclose(f_trass);
    free_students(studenty, kolvo);
    return 0;
}
