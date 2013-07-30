#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdbool.h>

#include "LineParser.h"

#define STDIN  0
#define STDOUT 1

#define BUF_SIZE 2048

#define ERROR "Error"


typedef struct envVars envVars;
struct envVars {
    char name[BUF_SIZE], value[BUF_SIZE];
    envVars *next;
};

int **createPipes(int nPipes) {
    int **pipes = malloc(nPipes * sizeof(int*)),
        *cPipe,
        i;

    for (i=0; i<nPipes; i++) {
        cPipe = malloc(2 * sizeof(int));
        if (pipe(cPipe) == -1) {
            perror("pipe(): failed executing command.");
        }
        pipes[i] = cPipe;
    }

    return pipes;
}

void releasePipes(int **pipes, int nPipes) {
    int i;

    for (i=0; i<nPipes; i++) {
        close(pipes[i][0]);
        close(pipes[i][1]);
        free(pipes[i]);
    }

    free(pipes);
}

int *feedPipe(int **pipes, cmdLine *pCmdLine) {
    if (pCmdLine->idx == 0) {
        return NULL;
    }

    return pipes[pCmdLine->idx -1];
}

int *sinkPipe(int **pipes, cmdLine *pCmdLine) {
    if (pCmdLine->next == NULL) {
        return NULL;
    }

    return pipes[pCmdLine->idx];
}

int execute(cmdLine *pCmdLine) {
    int i, c,
        cpid,
        status,
        **pipes,
        *cPipe;
    cmdLine *it = pCmdLine,
            *cmd = pCmdLine;

    c=0;
    while (it != NULL) {
        it = it->next;
        c++;
    }
    pipes = createPipes(c);

    i=0;
    while (i < c) {
        if ((cpid = fork()) == 0) {  /* fork each pipe/process. */
            if (c > 1) {  /* if pipes were found, do pipe. */
                if (cmd->idx != 0) {  /* redirect input to prev cmd if not 1st. */
                    cPipe = feedPipe(pipes, cmd);
                    close(STDIN);
                    dup2(cPipe[0], STDIN);
                    close(cPipe[0]);
                }
                if (cmd->next != NULL) {  /* redirect output to next cmd if not last. */
                    cPipe = sinkPipe(pipes, cmd);
                    close(STDOUT);
                    dup2(cPipe[1], STDOUT);
                    close(cPipe[1]);
                }
            }

            if (cmd->inputRedirect != NULL) {
                close(STDIN);
                fopen(cmd->inputRedirect, "r");
            }
            if (cmd->outputRedirect != NULL) {
                close(STDOUT);
                fopen(cmd->outputRedirect, "w");
            }

            if ((execvp(cmd->arguments[0], cmd->arguments)) != 0) {
                perror("execute(): failed executing command.");
            }

            return EXIT_SUCCESS;
        }

        cPipe = feedPipe(pipes, cmd);
        if (cPipe != NULL) {
            close(cPipe[0]);
        }

        cPipe = sinkPipe(pipes, cmd);
        if (cPipe != NULL) {
            close(cPipe[1]);
        }

        if (cmd->blocking == 1) {
            waitpid(cpid, &status, 0);
        }
        cmd = cmd->next;
        i++;
    }

    releasePipes(pipes, c - 1);

    return EXIT_SUCCESS;
}

/* Older tasks.
int execute2(cmdLine *pCmdLine) {
    int in, out,
        fildes[2], fildesCopy[2];
    pid_t child1, child2;

    if (pipe(fildes) != 0) {
        printf("Error: pipe().\n");
        exit(EXIT_FAILURE);
    }

    child1 = fork();
    if (child1 == 0) {
        if (pCmdLine->inputRedirect != NULL) {
            close(STDIN);
            in = fopen(pCmdLine->inputRedirect, "r");
            if (in == -1) {
                printf("Error: open(), inputRedirect.\n");
                return EXIT_FAILURE;
            }
        }

        if (close(STDOUT) == -1) {
            printf("Error: close(STDOUT).\n");
            exit(EXIT_FAILURE);
        }
        if ((fildesCopy[1] = dup(fildes[1])) == -1) {
            printf("Error: dup(fildes[1]).\n");
            exit(EXIT_FAILURE);
        }
        if (close(fildes[1]) == -1) {
            printf("Error: close(fildes[1]).\n");
            exit(EXIT_FAILURE);
        }
        if (execvp(pCmdLine->arguments[0], pCmdLine->arguments) == -1) {
            printf("Error: child1 execvp().\n");
            exit(EXIT_FAILURE);
        }
    } else {
        if (close(fildes[1]) == -1) {
            printf("Error: parent close(fildes[1]).\n");
            exit(EXIT_FAILURE);
        }
    }

    child2 = fork();
    if (child2 == 0) {
        if (pCmdLine->next->outputRedirect != NULL) {
            close(STDOUT);
            out = fopen(pCmdLine->next->outputRedirect, "w");
            if (out == -1) {
                printf("Error: open(), outputRedirect.\n");
                return EXIT_FAILURE;
            }
        }

        if (close(STDIN) == -1) {
            printf("Error: close(STDIN).\n");
            exit(EXIT_FAILURE);
        }
        if ((fildesCopy[0] = dup(fildes[0])) == -1) {
            printf("Error: dup(fildes[0]).\n");
            exit(EXIT_FAILURE);
        }
        if (close(fildes[0]) == -1) {
            printf("Error: close(fildes[0]).\n");
            exit(EXIT_FAILURE);
        }
        if (execvp(pCmdLine->next->arguments[0], pCmdLine->next->arguments) == -1) {
            printf("Error: child2 execvp().\n");
            exit(EXIT_FAILURE);
        }
    } else {
        if (close(fildes[0]) == -1) {
            printf("Error: parent close(fildes[0]).\n");
            exit(EXIT_FAILURE);
        }

        waitpid(child1, 0, 0);
        waitpid(child2, 0, 0);

        fclose(in);
        fclose(out);
    }

    return EXIT_SUCCESS;
}

int old_execute(cmdLine *pCmdLine) {
    int in, out;

    if (pCmdLine->next != NULL) {
        return execute2(pCmdLine);
    } else {
        if (pCmdLine->inputRedirect != NULL) {
            close(STDIN);
            in = open(pCmdLine->inputRedirect, O_RDONLY, S_IRUSR | S_IRGRP | S_IROTH);
            if (in == -1) {
                printf("Error: open(), inputRedirect.\n");
                return EXIT_FAILURE;
            }
        }

        if (pCmdLine->outputRedirect != NULL) {
            close(STDOUT);
            out = open(pCmdLine->outputRedirect, O_WRONLY, S_IWUSR | S_IWGRP | S_IWOTH);
            if (out == -1) {
                printf("Error: open(), outputRedirect.\n");
                return EXIT_FAILURE;
            }
        }

        return execvp(pCmdLine->arguments[0], pCmdLine->arguments);
    }
}
*/


int main (int argc, char* argv[]) {
    cmdLine *cmd;
    /* pid_t child; */
    envVars *envRoot = NULL,
            *env = NULL,
            *tmp;
    bool set;
    int i,j,
        h=0, hIndex,
        status;
    char wd[BUF_SIZE],
         in[BUF_SIZE],
         history[10][BUF_SIZE] = { "", "", "", "", "", "", "", "", "", ""};

    while (1) {
        getcwd(wd, BUF_SIZE);
        printf("%s $ ", wd);
        fgets(in, BUF_SIZE, stdin);

        /* Pre-checks. */

        if (strcmp(in, "quit\n") == 0) {
            break;
        } else if (in[0] == '!') {
            if (in[1] == 0) {
                printf("%s: No index.", ERROR);
            } else {
                hIndex = (atoi(&in[1]) +h-1) %10;
                if (hIndex >= 10 || strcmp(history[hIndex], "") == 0) {
                    printf("%s: Bad index.\n", ERROR);
                    continue;
                } else {
                    strcpy(in, history[hIndex]);
                }
            }
        }

        cmd = parseCmdLines(in);

        if (h == 10) {
            h=0;
        }
        strcpy(history[h++], in);

        for (i=1; i < cmd->argCount; i++) {
            if (cmd->arguments[i][0] == '$') {
                set = false;
                env = envRoot;
                while (env != NULL && ! set) {
                    if (strcmp(env->name, cmd->arguments[i] +1) == 0) {
                        replaceCmdArg(cmd, i, env->value);
                        set = true;
                    }
                    env = env->next;
                }

                if ( ! set) {
                    printf("Variable '%s' not set.\n", cmd->arguments[i]);
                }
            }
        }

        /* Specific cmds */

        if (strcmp(cmd->arguments[0], "cd") == 0) {
            if (cmd->argCount != 2) {
                printf("%s: Bad arguments.\n", ERROR);
            } else {
                status = chdir(cmd->arguments[1]);
                if (status != 0) {
                    perror(ERROR);
                }
            }
        } else if (strcmp(cmd->arguments[0], "mygecko") == 0) {
            for (i=0; i < cmd->argCount -1; i++) {
                printf("%s ", cmd->arguments[i+1]);
            }
            printf("\n");
        } else if (strcmp(cmd->arguments[0], "history") == 0) {
            if (strcmp(history[h], "") == 0) {
                j = 0;
            } else {
                j = h;
            }

            for (i=j; i < j+10; i++) {
                if (strcmp(history[i%10], "") == 0) {
                    break;
                }

                printf("%d:\t%s", i-j+1, history[i%10]);
            }
        } else if (strcmp(cmd->arguments[0], "set") == 0) {
            set = false;
            if (cmd->argCount != 3) {
                printf("%s: Bad arguments.", ERROR);
            } else {
                if (envRoot == NULL) {
                    if ((envRoot = (envVars *) malloc(sizeof(envVars))) == NULL) {
                        printf("%s: envRoot = malloc(..)", ERROR);
                    }
                    strcpy(envRoot->name, cmd->arguments[1]);
                    strcpy(envRoot->value, cmd->arguments[2]);
                    envRoot->next = NULL;
                } else {
                    env = envRoot;
                    while (env->next != NULL && ! set) {
                        if (strcmp(env->name, cmd->arguments[1]) == 0) {
                            strcpy(env->value, cmd->arguments[2]);
                            set = true;
                            break;
                        }
                        env = env->next;
                    }

                    if ( ! set) {
                        if (strcmp(env->name, cmd->arguments[1]) == 0) {
                            strcpy(env->value, cmd->arguments[2]);
                        } else {
                            if ((env->next = (envVars *) malloc(sizeof(envVars))) == NULL) {
                                printf("%s: env = malloc(..)", ERROR);
                            }
                            strcpy(env->next->name, cmd->arguments[1]);
                            strcpy(env->next->value, cmd->arguments[2]);
                            env->next->next = NULL;
                        }
                    }
                }
            }
        } else if (strcmp(cmd->arguments[0], "unset") == 0) {
            set = false;
            if (cmd->argCount != 2) {
                printf("%s: Bad arguments.", ERROR);
            } else {
                if (envRoot == NULL) {
                    printf("%s: Variable not set.", ERROR);
                } else {
                    if (strcmp(envRoot->name, cmd->arguments[1]) == 0) {
                        env = envRoot->next;

                        envRoot->next = NULL;
                        free(envRoot);

                        envRoot = env;
                    } else {
                        env = envRoot;
                        while (env->next != NULL && ! set) {
                            if (strcmp(env->next->name, cmd->arguments[1]) == 0) {
                                tmp = env->next->next;

                                env->next->next = NULL;
                                free(env->next);

                                env->next = tmp;
                                set = true;
                                break;
                            }
                            env = env->next;
                        }

                        if ( ! set) {
                            printf("%s: Variable not set.", ERROR);
                        }
                    }
                }
            }
        } else if (strcmp(cmd->arguments[0], "env") == 0) {
            env = envRoot;
            while (env != NULL) {
                printf("$%s=%s\n", env->name, env->value);
                env = env->next;
            }

        /* Other cmds */

        } else {
            status = execute(cmd);
            /*
             * child = fork();
             * if (child == 0) {
             *     status = old_execute(cmd);
             *     if (status != EXIT_SUCCESS) {
             *         perror(ERROR);
             *         _exit(status);
             *     }
             * } else {
             *     if ((int) cmd->blocking) {
             *         waitpid(child, &status, 0);
             *     }
             * }
             */
        }

        freeCmdLines(cmd);
        printf("\n");
    }

    if (envRoot != NULL) {  /* free env vars. */
        env = envRoot;
        while (env != NULL) {
            tmp = env->next;
            free(env);
            env = tmp;
        }
    }

    return EXIT_SUCCESS;
}
