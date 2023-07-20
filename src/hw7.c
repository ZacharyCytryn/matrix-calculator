#include "hw7.h"
int precedence(char operator);
int isOperator(char c);

bst_sf* insert_bst_sf(matrix_sf *mat, bst_sf *root) {
    if (root == NULL) {
        bst_sf *new_node = malloc(sizeof(bst_sf));
        new_node->mat = mat;
        new_node->left_child = NULL;
        new_node->right_child = NULL;
        return new_node;
    }

    if (mat->name < root->mat->name) {
        root->left_child = insert_bst_sf(mat, root->left_child);
    }
    else {
        root->right_child = insert_bst_sf(mat, root->right_child);
    }

    return root;
}

matrix_sf* find_bst_sf(char name, bst_sf *root) {
    if (root == NULL || root->mat->name == name) {
        return root == NULL ? NULL : root->mat;
    }

    if (name < root->mat->name) {
        return find_bst_sf(name, root->left_child);
    }
    else {
        return find_bst_sf(name, root->right_child);
    }
}

void free_bst_sf(bst_sf *root) {
    if (root == NULL) {
        return;
    }

    free_bst_sf(root->left_child);
    free_bst_sf(root->right_child);
    free(root->mat);
    free(root);
}

matrix_sf* add_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    matrix_sf *m = malloc(sizeof(matrix_sf) + mat1->num_rows*mat1->num_cols*sizeof(int));
    m->name = 'A';
    m->num_rows = mat1->num_rows;
    m->num_cols = mat1->num_cols;
    for (int i = 0; i < (mat1->num_rows * mat1->num_cols); i++) {
        m->values[i] = mat1->values[i] + mat2->values[i];
    }
    return m;
}

matrix_sf* mult_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
   matrix_sf *mult = malloc(sizeof(matrix_sf) + mat1->num_rows*mat2->num_cols*sizeof(int));
   mult->name = 'M';
   mult->num_rows = mat1->num_rows;
   mult->num_cols = mat2->num_cols;
   for (int i = 0; i < mat1->num_rows; i++) {
        for (int j = 0; j < mat2->num_cols; j++) {
            int sum = 0;
            for (int k = 0; k < mat1->num_cols; k++) {
                sum += mat1->values[i * mat1->num_cols + k] * mat2->values[k * mat2->num_cols + j];
            }
            mult->values[i * mat2->num_cols + j] = sum;
        }
   }
   return mult;
}

matrix_sf* transpose_mat_sf(const matrix_sf *mat) {
    matrix_sf *transpose = malloc(sizeof(matrix_sf) + mat->num_rows*mat->num_cols*sizeof(int));
    transpose->name = 'T';
    transpose->num_rows = mat->num_cols;
    transpose->num_cols = mat->num_rows;
    for (int i = 0; i < mat->num_rows; i++) {
        for (int j = 0; j < mat->num_cols; j++) {
            transpose->values[j * mat->num_rows + i] = mat->values[i * mat->num_cols + j];
        }
    }
    return transpose;
}

matrix_sf* create_matrix_sf(char name, const char *expr) {  
    int rows = 0;
    int cols = 0;
    int row_flag = 0;
    int col_flag = 0;
    int mat_ind = 0;
    int i_tracker = 0;
    int place_counter = 1;
    int open_flag = 0; //If matrix values are being written, this value is 1. Otherwise, it is 0;
    int start = 0; // index of expression showing beginning of matrix.

    for (int i = 0; i < strlen(expr); i++) {
        if (isdigit(expr[i])) {
            if (row_flag == 0) {
                while(!isspace(expr[i + 1]) && !ispunct(expr[i+1])) {
                    place_counter *= 10;
                    i_tracker++;
                    i++;
                }
                i -= i_tracker;

                while (!isspace(expr[i]) && !ispunct(expr[i])) {
                    rows += (expr[i] - '0') * place_counter;
                    place_counter /= 10;
                    i++;
                }
                row_flag = 1;
            }
            else {
                if (col_flag == 0) {
                    i_tracker = 0;
                    place_counter = 1;
                    while(!isspace(expr[i + 1]) && !ispunct(expr[i+1])) {
                        place_counter *= 10;
                        i_tracker++;
                        i++;
                    }
                    i -= i_tracker;

                    while(!isspace(expr[i]) && !ispunct(expr[i])) {
                        cols += (expr[i] - '0') * place_counter;
                        place_counter /= 10;
                        i++;
                    }
                    col_flag = 1;
                }
            }
            if(expr[i] == '[') {
                open_flag = 1;
                start = i;
                break;
            }
        }
        else {
            if (expr[i] == '[') {
                open_flag = 1;
                start = i;
                break;
            }
        }
    }

    matrix_sf *created = malloc(sizeof(matrix_sf) + rows*cols*sizeof(int));
    created->name = name;
    created->num_rows = rows;
    created->num_cols = cols;

    for (int i = start; i < strlen(expr); i++) {
        if (isdigit(expr[i])) {
            int neg_flag = 0; // if positive, 0. if negative, 1.
            if (expr[i - 1] == '-') {
                neg_flag = 1;
            }
            int val_sum = 0;
            place_counter = 1;
            i_tracker = 0;

            while(isdigit(expr[i + 1]) && i != strlen(expr)) {
                place_counter *= 10;
                i_tracker++;
                i++;
            }
            i -= i_tracker;

            while(isdigit(expr[i]) && i != strlen(expr)) {
                val_sum += (expr[i] - '0') * place_counter;
                place_counter /= 10;
                i++;
            }
            
            if (neg_flag == 1) {
                val_sum = -val_sum;
            }
            created->values[mat_ind++] = val_sum;
        }
        else {
            if (expr[i] == ']') {
                break;
            }
        }
    }
    return created;
}

int precedence(char operator) {
    switch (operator) {
        case '+':
            return 1;
        case '*':
            return 2;
        case '\'':
            return 3;
        default:
            return -1;
    }
}

int isOperator(char c) {
    return (c == '+' || c == '*' || c == '\'');
}

char* infix2postfix_sf(char *infix) {
    int i, j;
    char *postfix = (char *) malloc(sizeof(char) * (strlen(infix) + 2));
    char stack[100];
    int top = -1;

    for (i = 0, j = 0; i < strlen(infix); i++) {
        if (infix[i] == ' ' || infix[i] == '\t') {
            continue;
        }

        if (isalnum(infix[i])) {
            postfix[j++] = infix[i];
        }
        else {
            if (infix[i] == '(') {
                stack[++top] = infix[i];
            }
            else {
                if (infix[i] == ')') {
                    while (top > -1 && stack[top] != '(') {
                        postfix[j++] = stack[top--];
                    }
                    if(top > -1 && stack[top] != '(') {
                        return "Invalid Expression";
                    }
                    else {
                        top--;
                    }
                }
                else {
                    if(isOperator(infix[i])) {
                        while (top > -1 && precedence(stack[top]) >= precedence(infix[i])) {
                            postfix[j++] = stack[top--];
                        }
                        stack[++top] = infix[i];
                    }
                }
            }
        }
    }
    while (top > -1) {
        if (stack[top] == '(') {
            return "Invalid Expression";
        }
        postfix[j++] = stack[top--];
    }
    postfix[j] = '\0';
    return postfix;
}

matrix_sf* evaluate_expr_sf(char name, char *expr, bst_sf *root) {
    char *postfix = infix2postfix_sf(expr);
    matrix_sf *stack[100];
    int top = -1;
    int counter = 0;

    for(int i = 0; i < strlen(postfix); i++) {
        if (isalpha(postfix[i])) {
            stack[++top] = find_bst_sf(postfix[i], root);
        }
        else {
            if (isOperator(postfix[i])) {
                if (precedence(postfix[i]) == 3) {
                    matrix_sf *m = stack[top--];
                    matrix_sf *new_mat = transpose_mat_sf(m);
                    new_mat->name = '0' + counter++;
                    stack[++top] = new_mat;
                    if (m->name >= '0' && m->name <= '9') {
                        free(m);
                    }
                }
                else {
                    matrix_sf *second_m = stack[top--];
                    matrix_sf *first_m = stack[top--];
                    if (precedence(postfix[i]) == 2) {
                        matrix_sf *mult_mat = mult_mats_sf(first_m, second_m);
                        mult_mat->name = '0' + counter++;
                        stack[++top] = mult_mat;
                    }
                    else {
                        matrix_sf *add_mat = add_mats_sf(first_m, second_m);
                        add_mat->name = '0' + counter++;
                        stack[++top] = add_mat;
                    }
                    if (first_m->name >= '0' && first_m->name <= '9') {
                        free(first_m);
                    }
                    if (second_m->name >= '0' && second_m->name <= '9') {
                        free(second_m);
                    }
                }
            }
        }
    }
    stack[top]->name = name;
    free(postfix);
    return stack[top];
}

matrix_sf *execute_script_sf(char *filename) {
    FILE *file = fopen(filename, "r");
    char *lineptr = NULL;
    size_t max_line_size = MAX_LINE_LEN;
    
    int count = 0;
    char c;
    while (1) {
        if (feof(file)) {
            count++;
            break;
        }
        c = fgetc(file);
        if (c == '\n' || c == '\0') {
            count++;
        }
    }
    rewind(file);

    bst_sf *root = NULL;
    matrix_sf *fin = NULL;
    for (int i = 0; i < count; i++) {
        int matrix_flag = 0; // 1 if a matrix is being created, 0 otherwise.
        int expr_flag = 0; // 1 if evaluating an expressiong, 0 otherwise.
        int equal_ind = 0;
        char name;
        getline(&lineptr, &max_line_size, file);
        int j = 0;
        while (lineptr[j] != '=') {
            j++;
        }
        equal_ind = j;
        while (matrix_flag == 0 && expr_flag == 0) {
            if (isdigit(lineptr[j])) {
                matrix_flag = 1;
            }
            else {
                if (isalpha(lineptr[j])) {
                    expr_flag = 1;
                }
            }
            j++;
        }
        for (int k = 0; k < equal_ind; k++) {
            if (isalpha(lineptr[k])) {
                name = lineptr[k];
            }
        }
        if (matrix_flag == 1) {
            matrix_sf *cm = create_matrix_sf(name, lineptr);
            root = insert_bst_sf(cm, root);
        }
        else {
            matrix_sf *ce = evaluate_expr_sf(name, lineptr, root);
            root = insert_bst_sf(ce, root);
        }

        if (i == (count - 1)) {
            matrix_sf *temp = find_bst_sf(name, root);
            fin = copy_matrix(temp->num_rows, temp->num_cols, temp->values);
            fin->name = temp->name;
        }
    }
    fclose(file);
    free(lineptr);
    free_bst_sf(root);
    return fin;
}

// This is a utility function used during testing. Feel free to adapt the code to implement some of
// the assignment. Feel equally free to ignore it.
matrix_sf *copy_matrix(unsigned int num_rows, unsigned int num_cols, int values[]) {
    matrix_sf *m = malloc(sizeof(matrix_sf)+num_rows*num_cols*sizeof(int));
    m->name = '?';
    m->num_rows = num_rows;
    m->num_cols = num_cols;
    memcpy(m->values, values, num_rows*num_cols*sizeof(int));
    return m;
}

// Don't touch this function. It's used by the testing framework.
// It's been left here in case it helps you debug and test your code.
void print_matrix_sf(matrix_sf *mat) {
    assert(mat != NULL);
    assert(mat->num_rows <= 1000);
    assert(mat->num_cols <= 1000);
    printf("%d %d ", mat->num_rows, mat->num_cols);
    for (unsigned int i = 0; i < mat->num_rows*mat->num_cols; i++) {
        printf("%d", mat->values[i]);
        if (i < mat->num_rows*mat->num_cols-1)
            printf(" ");
    }
    printf("\n");
}
