#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//  you may need other standard header files


//  CITS2002 Project 1 2023
//  Student1:   23751337   JIA QI LAM
//  Student2:   23970936   JACOB READ


//  myscheduler (v1.0)
//  Compile with:  cc -std=c11 -Wall -Werror -o myscheduler myscheduler.c


//  THESE CONSTANTS DEFINE THE MAXIMUM SIZE OF sysconfig AND command DETAILS
//  THAT YOUR PROGRAM NEEDS TO SUPPORT.  YOU'LL REQUIRE THESE //  CONSTANTS
//  WHEN DEFINING THE MAXIMUM SIZES OF ANY REQUIRED DATA STRUCTURES.

#define MAX_DEVICES                     4
#define MAX_DEVICE_NAME                 20
#define MAX_COMMANDS                    10
#define MAX_COMMAND_NAME                20
#define MAX_SYSCALLS_PER_PROCESS        40
#define MAX_RUNNING_PROCESSES           50

//  NOTE THAT DEVICE DATA-TRANSFER-RATES ARE MEASURED IN BYTES/SECOND,
//  THAT ALL TIMES ARE MEASURED IN MICROSECONDS (usecs),
//  AND THAT THE TOTAL-PROCESS-COMPLETION-TIME WILL NOT EXCEED 2000 SECONDS
//  (SO YOU CAN SAFELY USE 'STANDARD' 32-BIT ints TO STORE TIMES).

#define DEFAULT_TIME_QUANTUM            100

#define TIME_CONTEXT_SWITCH             5
#define TIME_CORE_STATE_TRANSITIONS     10
#define TIME_ACQUIRE_BUS                20


//  ----------------------------------------------------------------------

#define CHAR_COMMENT                    '#'

struct device {
    char *name;
    int read_speed;
    int write_speed;
    struct process *queue_head;
    struct process *queue_tail;
    struct device *next;
};

struct process {
    struct command *command;
    int time;
    int sycall_index;
    int num_children;
    struct process *parent;
    struct process *next;
};

struct command {
    char *name;
    struct syscall *queue_head; 
    struct syscall *queue_tail;
    struct command *next;
};

enum syscall_types {
    SPAWN,
    READ,
    WRITE,
    SLEEP,
    WAIT,
    EXIT
};

struct syscall {
    int time;
    enum syscall_types type;
    struct device *device;
    struct command *command;
    int data;
    struct syscall *next;
};

struct sleeping {
    struct process *process;
    int time;
    struct sleeping *next;
};

struct device *device1; // A pointer to the first device

struct command *command1; // A pointer to the first command
struct command *commandn; // A pointer to the last command

struct process *ready1; // A pointer to the first ready process
struct process *readyn; // A pointer to the last ready process

struct process *running; // A pointer to the running process

struct sleeping *sleeping1; // A pointer to the first sleeping process

struct process *waiting1; // A pointer to the first waiting process

struct sleeping *bus_process; // A pointer to the process that is using the bus

struct device *create_device(char *name, int read_speed, int write_speed) {
    // Adds a new device to the linked list of devices, ordered by read speed (descending)
    struct device *new_device = malloc(sizeof(struct device));
    strcpy(new_device->name, name);
    new_device->read_speed = read_speed;
    new_device->write_speed = write_speed;
    new_device->queue_head = NULL;
    new_device->queue_tail = NULL;
    new_device->next = NULL;
    if (device1 == NULL) {
        device1 = new_device;
    } else {
        struct device *current = device1;
        struct device *previous = NULL;
        while (current != NULL) {
            if (read_speed > current->read_speed) {
                if (previous == NULL) {
                    new_device->next = device1;
                    device1 = new_device;
                } else {
                    previous->next = new_device;
                    new_device->next = current;
                }
                break;
            }
            previous = current;
            current = current->next;
        }
        if (current == NULL) {
            previous->next = new_device;
        }
    }
}

struct process *create_process(struct command *command, struct process *parent) {
    // Adds a new process to the end of the ready linked list
    struct process *new_process = malloc(sizeof(struct process));
    new_process->command = command;
    new_process->time = 0;
    new_process->sycall_index = 0;
    new_process->num_children = 0;
    new_process->parent = parent;
    new_process->next = NULL;
    if (ready1 == NULL) {
        ready1 = new_process;
        readyn = new_process;
    } else {
        readyn->next = new_process;
        readyn = new_process;
    }
}

struct command *create_command(char *name) {
    // Adds a new command to the end of the command linked list
    struct command *new_command = malloc(sizeof(struct command));
    strcpy(new_command->name, name);
    new_command->queue_head = NULL;
    new_command->queue_tail = NULL;
    new_command->next = NULL;
    if (command1 == NULL) {
        command1 = new_command;
        commandn = new_command;
    } else {
        commandn->next = new_command;
        commandn = new_command;
    }
}

struct syscall *create_syscall(struct command *parent_command, int time, enum syscall_types type, struct device *device, struct command *command, int data) {
    // Adds a new syscall to the end of the parent command's syscall linked list
    struct syscall *new_syscall = malloc(sizeof(struct syscall));
    new_syscall->time = time;
    new_syscall->type = type;
    new_syscall->device = device;
    new_syscall->command = command;
    new_syscall->data = data;
    new_syscall->next = NULL;
    if (parent_command->queue_head == NULL) {
        parent_command->queue_head = new_syscall;
        parent_command->queue_tail = new_syscall;
    } else {
        parent_command->queue_tail->next = new_syscall;
        parent_command->queue_tail = new_syscall;
    }
}

struct sleeping *create_sleeping(struct process *process, int time) {
    // Adds a new sleeping process to the sleeping linked list in order of time (ascending)
    struct sleeping *new_sleeping = malloc(sizeof(struct sleeping));
    new_sleeping->process = process;
    new_sleeping->time = time;
    new_sleeping->next = NULL;
    if (sleeping1 == NULL) {
        sleeping1 = new_sleeping;
    } else {
        struct sleeping *current = sleeping1;
        struct sleeping *previous = NULL;
        while (current != NULL) {
            if (time < current->time) {
                if (previous == NULL) {
                    new_sleeping->next = sleeping1;
                    sleeping1 = new_sleeping;
                } else {
                    previous->next = new_sleeping;
                    new_sleeping->next = current;
                }
                break;
            }
            previous = current;
            current = current->next;
        }
        if (current == NULL) {
            previous->next = new_sleeping;
        }
    }
}

int time_quantum = DEFAULT_TIME_QUANTUM;

void read_sysconfig(char argv0[], char filename[]) {
    FILE *file = fopen(filename, "r");
    if (file == NULL) {
        printf("Failed to open file: %s\n", filename);
        return;
    }
    char line[100];
    while (fgets(line, sizeof(line), file) != NULL) {
        if (line[0] == CHAR_COMMENT || line[0] == '\n') {
            continue; // Skip comment lines and empty lines
        }
        char *type;
        sscanf(line, "%s", type);
        if (strcmp(type, "device") == 0) {
            char *name;
            int read_speed;
            int write_speed;
            sscanf(line, "%s %s %dBps %dBps", type, name, &read_speed, &write_speed);
            create_device(name, read_speed, write_speed);
        } else if (strcmp(type, "timequantum") == 0) {
            int time;
            sscanf(line, "%s %d", type, &time);
            time_quantum = time;
        } else {
            printf("Invalid sysconfig: %s\n", type);
        }
    }
}

void read_commands(char argv0[], char filename[]) {
    FILE *file = fopen(filename, "r");
    if (file == NULL) {
        printf("Failed to open file: %s\n", filename);
        return;
    }
    char line[100];
    struct command *current_command;
    while (fgets(line, sizeof(line), file) != NULL) {
        if (line[0] == CHAR_COMMENT || line[0] == '\n') {
            continue; // Skip comment lines and empty lines
        }
        if (line[0] == '\t') {
            int time;
            char *type;                                             
            sscanf(line, "%dusecs %s", &time, type);
            if (strcmp(type, "spawn") == 0) {
                char *name;
                sscanf(line, "%dusecs %s %s", &time, type, name);
                // Check if the command already exists
                struct command *command = command1;
                while (command != NULL) {
                    if (strcmp(command->name, name) == 0) {
                        create_syscall(current_command, time, SPAWN, NULL, command, 0);
                        break;
                    }
                    command = command->next;
                }
                if (command == NULL) {
                    create_command(name);
                    create_syscall(current_command, time, SPAWN, NULL, commandn, 0);
                }
            } else if (strcmp(type, "read") == 0 || strcmp(type, "write") == 0) {
                int data;
                char *name;
                sscanf(line, "%dusecs %s %s %dB", &time, type, name, &data);
                // Check if the device already exists
                struct device *device = device1;
                while (device != NULL) {
                    if (strcmp(device->name, name) == 0) {
                        if (strcmp(type, "read") == 0) {
                            create_syscall(current_command, time, READ, device, NULL, data);
                        } else {
                            create_syscall(current_command, time, WRITE, device, NULL, data);
                        }
                        break;
                    }
                    device = device->next;
                }
                if (device == NULL) {
                    printf("Device not found: %s\n", name);
                }
            } else if (strcmp(type, "sleep") == 0) {
                int data;
                sscanf(line, "%dusecs %s %dusecs", &time, type, &data);
                create_syscall(current_command, time, SLEEP, NULL, NULL, data);
            } else if (strcmp(type, "wait") == 0 || strcmp(type, "exit") == 0) {
                sscanf(line, "%dusecs %s", &time, type);
                if (strcmp(type, "wait") == 0) {
                    create_syscall(current_command, time, WAIT, NULL, NULL, 0);
                } else {
                    create_syscall(current_command, time, EXIT, NULL, NULL, 0);
                }
            } else {
                printf("Invalid syscall: %s\n", type);
            }
        } else {
            char *name;
            sscanf(line, "%s", name);
            // Check if the command already exists
            struct command *command = command1;
            while (command != NULL) {
                if (strcmp(command->name, name) == 0) {
                    current_command = command;
                    break;
                }
                command = command->next;
            }
            if (command == NULL) {
                create_command(name);
                current_command = commandn;
            }      
        }
    }
    fclose(file);
}

//  ----------------------------------------------------------------------

void execute_commands(void) {

}

//  ----------------------------------------------------------------------

int main(int argc, char *argv[]) {
//  ENSURE THAT WE HAVE THE CORRECT NUMBER OF COMMAND-LINE ARGUMENTS
    if(argc != 3) {
        printf("Usage: %s sysconfig-file command-file\n", argv[0]);
        exit(EXIT_FAILURE);
    }

//  READ THE SYSTEM CONFIGURATION FILE
    read_sysconfig(argv[0], argv[1]);

//  READ THE COMMAND FILE
    read_commands(argv[0], argv[2]);

    // DEBUG

    struct device *device = device1;
    while (device != NULL) {
        printf("Device: %s, Read Speed: %dBps, Write Speed: %dBps\n", device->name, device->read_speed, device->write_speed);
        device = device->next;
    }
    struct command *command = command1;
    while (command != NULL) {
        printf("Command: %s\n", command->name);
        struct syscall *syscall = command->queue_head;
        while (syscall != NULL) {
            printf("Time: %d, Type: %d, Device: %s, Command: %s, Data: %d\n", syscall->time, syscall->type, syscall->device->name, syscall->command->name, syscall->data);
            syscall = syscall->next;
        }
        command = command->next;
    }
    // print timequantum
    printf("Time Quantum: %d\n", time_quantum);

//  EXECUTE COMMANDS, STARTING AT FIRST IN command-file, UNTIL NONE REMAIN
    execute_commands();

//  PRINT THE PROGRAM'S RESULTS
    printf("measurements  %i  %i\n", 0, 0);

    exit(EXIT_SUCCESS);
    return 0;
}

//  vim: ts=8 sw=4
