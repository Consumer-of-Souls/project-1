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
    // A struct to represent a device
    char *name; // The name of the device
    int read_speed; // The read speed of the device
    int write_speed; // The write speed of the device
    struct process *queue_head; // A pointer to the head of the queue of processes waiting to use the device
    struct process *queue_tail; // A pointer to the tail of the queue of processes waiting to use the device
    struct device *next; // A pointer to the next device in the linked list
};

struct process { 
    // A struct to represent a process
    struct command *command; // A pointer to the command that the process is executing
    int time; // The time that the process has been running for
    int num_children; // The number of children that the process has
    struct process *parent; // A pointer to the parent of the process
    struct process *next; // A pointer to the next process in the linked list
};

struct command {
    // A struct to represent a command
    char *name; // The name of the command
    struct syscall *queue_head; // A pointer to the head of the queue of syscalls that the command needs to execute
    struct syscall *queue_tail; // A pointer to the tail of the queue of syscalls that the command needs to execute
    struct command *next; // A pointer to the next command in the linked list
};

enum syscall_types {
    // An enum to represent the type of a syscall
    SPAWN, // Spawn a new process
    READ, // Read from a device
    WRITE, // Write to a device
    SLEEP, // Sleep for a certain amount of time
    WAIT, // Wait for a child process to finish
    EXIT // Exit the process
};

struct syscall {
    // A struct to represent a syscall
    int time; // The time that the syscall needs to be executed (cumulative)
    enum syscall_types type; // The type of the syscall
    struct device *device; // A pointer to the device that the syscall needs to use
    struct command *command; // A pointer to the command that the syscall needs to spawn
    int data; // The data that the syscall needs to read or write or the time that the process needs to sleep for
    struct syscall *next; // A pointer to the next syscall in the linked list
};

struct sleeping {
    // A struct to represent a sleeping process or the process that is using the bus
    struct process *process; // A pointer to the process that is sleeping or using the bus
    int time; // The time at which the process will wake up or finish using the bus
    struct sleeping *next; // A pointer to the next sleeping process in the linked list
};

struct device *device1 = NULL; // A pointer to the first device

struct command *command1 = NULL; // A pointer to the first command
struct command *commandn = NULL; // A pointer to the last command

struct process *ready1 = NULL; // A pointer to the first ready process
struct process *readyn = NULL; // A pointer to the last ready process

struct process *running = NULL; // A pointer to the running process

struct sleeping *sleeping1 = NULL; // A pointer to the first sleeping process

struct process *waiting1 = NULL; // A pointer to the first waiting process

struct sleeping *bus_process = NULL; // A pointer to the process that is using the bus

int create_device(char *name, int read_speed, int write_speed) {
    // Adds a new device to the linked list of devices, ordered by read speed (descending)
    struct device *new_device = malloc(sizeof(struct device)); // Allocate memory for the new device
    new_device->name = name; // Set the name of the new device
    new_device->read_speed = read_speed; // Set the read speed of the new device
    new_device->write_speed = write_speed; // Set the write speed of the new device
    new_device->queue_head = NULL; // Set the head of the queue of processes waiting to use the device to NULL
    new_device->queue_tail = NULL; // Set the tail of the queue of processes waiting to use the device to NULL
    new_device->next = NULL; // Set the next device in the linked list to NULL
    if (device1 == NULL) {
        device1 = new_device; // If the linked list of devices is empty, set the first device to the new device
    } else {
        struct device *current = device1; // Set the current device to the first device in the linked list
        struct device *previous = NULL; // Set the previous device to NULL
        while (current != NULL) {
            if (read_speed > current->read_speed) {
                // If the read speed of the new device is higher than the current device in the linked list, insert the new device before the current device
                if (previous == NULL) {
                    // If the new device has the highest read speed, set it as the first device
                    new_device->next = device1; // Set the next device of the new device to the first device
                    device1 = new_device; // Set the first device to the new device
                } else {
                    // If the new device has a read speed higher than the current device, but not the highest read speed, insert the new device before the current device
                    previous->next = new_device; // Set the next device of the previous device to the new device
                    new_device->next = current; // Set the next device of the new device to the current device
                }
                break;
            }
            previous = current; // Set the previous device to the current device
            current = current->next; // Set the current device to the next device in the linked list
        }
        if (current == NULL) {
            previous->next = new_device; // If the new device has the lowest read speed, set it as the last device
        }
    }
    return 0; // Return 0 to indicate success
}

int create_process(struct command *command, struct process *parent) {
    // Adds a new process to the end of the ready linked list
    struct process *new_process = malloc(sizeof(struct process)); // Allocate memory for the new process
    new_process->command = command; // Set the command the new process is executing
    new_process->time = 0; // Set the time of the new process to 0
    new_process->num_children = 0; // Set the number of children of the new process to 0
    new_process->parent = parent; // Set the parent of the new process
    new_process->next = NULL; // Set the next process in the linked list to NULL
    if (ready1 == NULL) {
        ready1 = new_process; // If the linked list of ready processes is empty, set the first ready process to the new process
        readyn = new_process; // Set the last ready process to the new process
    } else {
        readyn->next = new_process; // Set the next process of the last ready process to the new process
        readyn = new_process; // Set the last ready process to the new process
    }
    return 0; // Return 0 to indicate success
}

int create_command(char *name) {
    // Adds a new command to the end of the command linked list
    struct command *new_command = malloc(sizeof(struct command)); // Allocate memory for the new command
    new_command->name = name; // Set the name of the new command
    new_command->queue_head = NULL; // Set the head of the queue of syscalls that the command needs to execute to NULL
    new_command->queue_tail = NULL; // Set the tail of the queue of syscalls that the command needs to execute to NULL
    new_command->next = NULL; // Set the next command in the linked list to NULL
    if (command1 == NULL) {
        command1 = new_command; // If the linked list of commands is empty, set the first command to the new command
        commandn = new_command; // Set the last command to the new command
    } else {
        commandn->next = new_command; // Set the next command of the last command to the new command
        commandn = new_command; // Set the last command to the new command
    }
    return 0; // Return 0 to indicate success
}

int create_syscall(struct command *parent_command, int time, enum syscall_types type, struct device *device, struct command *command, int data) {
    // Adds a new syscall to the end of the parent command's syscall linked list
    struct syscall *new_syscall = malloc(sizeof(struct syscall)); // Allocate memory for the new syscall
    new_syscall->time = time; // Set the time the syscall takes to execute
    new_syscall->type = type; // Set the type of the syscall
    new_syscall->device = device; // Set the device that the syscall needs to use
    new_syscall->command = command; // Set the command that the syscall needs to spawn
    new_syscall->data = data; // Set the data that the syscall needs to read or write or the time that the process needs to sleep for
    new_syscall->next = NULL; // Set the next syscall in the linked list to NULL
    if (parent_command->queue_head == NULL) {
        parent_command->queue_head = new_syscall; // If the linked list of syscalls for the parent command is empty, set the head of the queue to the new syscall
        parent_command->queue_tail = new_syscall; // Set the tail of the queue to the new syscall
    } else {
        parent_command->queue_tail->next = new_syscall; // Set the next syscall of the tail of the parent command's queue to the new syscall
        parent_command->queue_tail = new_syscall; // Set the tail of the queue to the new syscall
    }
    return 0; // Return 0 to indicate success
}

int create_sleeping(struct process *process, int time) {
    // Adds a new sleeping process to the sleeping linked list in order of time (ascending)
    struct sleeping *new_sleeping = malloc(sizeof(struct sleeping)); // Allocate memory for the new sleeping process
    new_sleeping->process = process; // Set the process that is sleeping
    new_sleeping->time = time; // Set the time at which the process will wake up
    new_sleeping->next = NULL; // Set the next sleeping process in the linked list to NULL
    if (sleeping1 == NULL) {
        sleeping1 = new_sleeping; // If the linked list of sleeping processes is empty, set the first sleeping process to the new sleeping process
    } else {
        struct sleeping *current = sleeping1; // Set the current sleeping process to the first sleeping process in the linked list
        struct sleeping *previous = NULL; // Set the previous sleeping process to NULL
        while (current != NULL) {
            if (time < current->time) {
                // If the time of the new sleeping process is lower than the current sleeping process in the linked list, insert the new sleeping process before the current sleeping process
                if (previous == NULL) {
                    // If the new sleeping process has the lowest time, set it as the first sleeping process
                    new_sleeping->next = sleeping1; // Set the next sleeping process of the new sleeping process to the first sleeping process
                    sleeping1 = new_sleeping; // Set the first sleeping process to the new sleeping process
                } else {
                    // If the new sleeping process has a time lower than the current sleeping process, but not the lowest time, insert the new sleeping process before the current sleeping process
                    previous->next = new_sleeping; // Set the next sleeping process of the previous sleeping process to the new sleeping process
                    new_sleeping->next = current; // Set the next sleeping process of the new sleeping process to the current sleeping process
                }
                break;
            }
            previous = current; // Set the previous sleeping process to the current sleeping process
            current = current->next; // Set the current sleeping process to the next sleeping process in the linked list
        }
        if (current == NULL) {
            previous->next = new_sleeping; // If the new sleeping process has the highest time, set it as the last sleeping process
        }
    }
    return 0; // Return 0 to indicate success
}

int time_quantum = DEFAULT_TIME_QUANTUM; // The time quantum of the system

void read_sysconfig(char argv0[], char filename[]) {
    // Reads the sysconfig file and creates the devices and sets the time quantum
    FILE *file = fopen(filename, "r"); // Open the sysconfig file
    if (file == NULL) {
        printf("Failed to open file: %s\n", filename); // Print an error message if the file cannot be opened
        return;
    }
    char line[100]; // A string to store each line of the file
    while (fgets(line, sizeof(line), file) != NULL) {
        // Read each line of the file
        if (line[0] == CHAR_COMMENT || line[0] == '\n') {
            continue; // Skip comment lines and empty lines
        }
        char *type = malloc(12); // A string to store the type (device or timequantum)
        sscanf(line, "%s", type); // Read the type from the line
        if (strcmp(type, "device") == 0) {
            // If the type is device, create a new device
            char *name = malloc(MAX_DEVICE_NAME+1); // A string to store the name of the device
            int read_speed; // An int to store the read speed of the device
            int write_speed; // An int to store the write speed of the device
            sscanf(line, "%s %s %dBps %dBps", type, name, &read_speed, &write_speed); // Read the name, read speed and write speed from the line
            create_device(name, read_speed, write_speed); // Create the device
        } else if (strcmp(type, "timequantum") == 0) {
            // If the type is timequantum, set the time quantum
            int time; // An int to store the time quantum
            sscanf(line, "%s %d", type, &time); // Read the time quantum from the line
            time_quantum = time; // Set the time quantum
        } else {
            printf("Invalid sysconfig: %s\n", type); // Print an error message if the type is invalid
        }
    }
    fclose(file); // Close the sysconfig file
}

void read_commands(char argv0[], char filename[]) {
    // Reads the command file and creates the commands and their syscalls
    FILE *file = fopen(filename, "r"); // Open the command file
    if (file == NULL) {
        printf("Failed to open file: %s\n", filename); // Print an error message if the file cannot be opened
        return;
    }
    char line[100]; // A string to store each line of the file
    struct command *current_command = NULL; // A pointer to the current command
    while (fgets(line, sizeof(line), file) != NULL) {
        // Read each line of the file
        if (line[0] == CHAR_COMMENT || line[0] == '\n') {
            continue; // Skip comment lines and empty lines
        }
        if (line[0] == '\t') {
            // If the line starts with a tab, create a new syscall
            int time; // An int to store the time that the syscall needs to be executed
            char *type = malloc(6); // A string to store the type of the syscall                                     
            sscanf(line, "%dusecs %s", &time, type); // Read the time and type of the syscall from the line
            if (strcmp(type, "spawn") == 0) {
                // If the type is spawn, read the name of the command that needs to be spawned
                char *name = malloc(MAX_COMMAND_NAME+1); // A string to store the name of the command
                sscanf(line, "%dusecs %s %s", &time, type, name); // Read the name of the command from the line
                // Check if the command to be spawned already exists
                struct command *command = command1; // Set command to the first command in the linked list
                while (command != NULL) {
                    if (strcmp(command->name, name) == 0) {
                        // If the command already exists, create the syscall
                        create_syscall(current_command, time, SPAWN, NULL, command, 0); // Create the syscall
                        break;
                    }
                    command = command->next; // Set command to the next command in the linked list
                }
                if (command == NULL) {
                    // If the command does not exist, create the command and the syscall
                    create_command(name); // Create the command
                    create_syscall(current_command, time, SPAWN, NULL, commandn, 0); // Create the syscall
                }
            } else if (strcmp(type, "read") == 0 || strcmp(type, "write") == 0) {
                // If the type is read or write, read the name of the device that needs to be read from or written to and the data that needs to be read or written
                int data; // An int to store the data that needs to be read or written
                char *name = malloc(MAX_DEVICE_NAME+1); // A string to store the name of the device
                sscanf(line, "%dusecs %s %s %dB", &time, type, name, &data); // Read the name of the device and the data from the line
                // Check if the device to be read from or written to exists
                struct device *device = device1; // Set the current device to the first device in the linked list
                while (device != NULL) {
                    if (strcmp(device->name, name) == 0) {
                        // If the device exists, create the syscall
                        if (strcmp(type, "read") == 0) {
                            create_syscall(current_command, time, READ, device, NULL, data); // If the type is read, create a read syscall
                        } else {
                            create_syscall(current_command, time, WRITE, device, NULL, data); // If the type is write, create a write syscall
                        }
                        break;
                    }
                    device = device->next; // Set the current device to the next device in the linked list
                }
                if (device == NULL) {
                    printf("Device not found: %s\n", name); // Print an error message if the device does not exist
                }
            } else if (strcmp(type, "sleep") == 0) {
                // If the type is sleep, read the time that the process needs to sleep for
                int data; // An int to store the time that the process needs to sleep for
                sscanf(line, "%dusecs %s %dusecs", &time, type, &data); // Read the time that the process needs to sleep for from the line
                create_syscall(current_command, time, SLEEP, NULL, NULL, data); // Create the syscall
            } else if (strcmp(type, "wait") == 0) {
                // If the type is wait, create the syscall
                create_syscall(current_command, time, WAIT, NULL, NULL, 0);
            } else if (strcmp(type, "exit") == 0) {
                // If the type is exit, create the syscall
                create_syscall(current_command, time, EXIT, NULL, NULL, 0);
            } else {
                printf("Invalid syscall: %s\n", type); // Print an error message if the type is invalid
            }
        } else {
            // If the line does not start with a tab, create a new command
            char *name = malloc(MAX_COMMAND_NAME+1); // A string to store the name of the command
            sscanf(line, "%s", name); // Read the name of the command from the line
            // Check if the command already exists
            struct command *command = command1; // Set command to the first command in the linked list
            while (command != NULL) {
                if (strcmp(command->name, name) == 0) {
                    current_command = command; // If the command already exists, set the current command to the command
                    break;
                }
                command = command->next; // Set command to the next command in the linked list
            }
            if (command == NULL) {
                // If the command does not exist, create the command
                create_command(name); // Create the command
                current_command = commandn; // Set the current command to the command
            }      
        }
    }
    fclose(file); // Close the command file
}

//  ----------------------------------------------------------------------

int system_time = 0; // The current system time

void execute_commands(void) {
    //Need to check current running process, the sleeping processes and the process on the data-bus (keep sleeping and data-bus in the sleeping linked list)
}

//  ----------------------------------------------------------------------

int main(int argc, char *argv[]) {
    // ENSURE THAT WE HAVE THE CORRECT NUMBER OF COMMAND-LINE ARGUMENTS
    if(argc != 3) {
        printf("Usage: %s sysconfig-file command-file\n", argv[0]); // Print an error message if the number of command-line arguments is incorrect
        exit(EXIT_FAILURE); // EXIT WITH A NON-ZERO EXIT CODE TO INDICATE AN ERROR
        return 1; // Return 1 to indicate an error
    }

    // READ THE SYSTEM CONFIGURATION FILE
    read_sysconfig(argv[0], argv[1]);

    // READ THE COMMAND FILE
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
            printf("Time: %d, Type: %d\n", syscall->time, syscall->type);
            syscall = syscall->next;
        }
        command = command->next;
    }
    printf("Time Quantum: %d\n", time_quantum);

    // END DEBUG

    // EXECUTE COMMANDS, STARTING AT FIRST IN command-file, UNTIL NONE REMAIN
    execute_commands();

    // PRINT THE PROGRAM'S RESULTS
    printf("measurements  %i  %i\n", 0, 0);

    exit(EXIT_SUCCESS); // EXIT WITH A SUCCESSFUL RETURN CODE
    return 0; // Return 0 to indicate success
}

//  vim: ts=8 sw=4
