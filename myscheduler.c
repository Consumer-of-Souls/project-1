#include <stdio.h>
#include <stdlib.h>
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
    char name[MAX_DEVICE_NAME+1];
    int read_speed;
    int write_speed;
};

struct process {
    struct command *command;
    int time;
    struct process **children;
    int num_children;
    struct process *parent;
};

struct command {
    char name[MAX_COMMAND_NAME+1];
    struct syscall **syscalls;          // check if just one asterisk - pointer to array of structs or pointers
    int num_syscalls;
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
    int data;
};

struct device *create_device(char *name, int read_speed, int write_speed) {
    struct device *new_device = malloc(sizeof(struct device));
    strcpy(new_device->name, name);
    new_device->read_speed = read_speed;
    new_device->write_speed = write_speed;
    return new_device;
}

struct process *create_process(struct command *command, int time, struct process *parent) {
    struct process *new_process = malloc(sizeof(struct process));
    new_process->command = command;
    new_process->time = time;
    new_process->children = NULL;
    new_process->num_children = 0;
    new_process->parent = parent;
    return new_process;
}

struct command *create_command(char *name, struct syscall **syscalls, int num_syscalls) {
    struct command *new_command = malloc(sizeof(struct command));
    strcpy(new_command->name, name);
    new_command->syscalls = syscalls;
    new_command->num_syscalls = num_syscalls;
    return new_command;
}

struct syscall *create_syscall(int time, enum syscall_types type, struct device *device, int data) {
    struct syscall *new_syscall = malloc(sizeof(struct syscall));
    new_syscall->time = time;
    new_syscall->type = type;
    new_syscall->device = device;
    new_syscall->data = data;
    return new_syscall;
}

void read_sysconfig(char argv0[], char filename[]) {

}

void read_commands(char argv0[], char filename[]) {

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

//  EXECUTE COMMANDS, STARTING AT FIRST IN command-file, UNTIL NONE REMAIN
    execute_commands();

//  PRINT THE PROGRAM'S RESULTS
    printf("measurements  %i  %i\n", 0, 0);

    exit(EXIT_SUCCESS);
}

//  vim: ts=8 sw=4
