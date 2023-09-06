#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>

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

#define DEFAULT_TIME_QUANTUM               100

#define TIME_CONTEXT_SWITCH                5
#define TIME_CORE_STATE_TRANSITIONS        10
#define TIME_ACQUIRE_BUS                   20
#define ADDITIONAL_SYSCALL_EXECUTION_TIME  1


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
    struct syscall *syscall; // A pointer to the last syscall that the process executed
    int time; // The time that the process has been running for
    int num_children; // The number of children that the process has
    int waiting_bool; // A boolean to indicate if the process is waiting for a child process to finish
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

struct bus {
    // A struct to represent the bus
    struct process *process; // A pointer to the process that is using the bus
    int time; // The time at which the process will finish using the bus
};


struct device *device1 = NULL; // A pointer to the first device in the linked list of devices

struct command *command1 = NULL; // A pointer to the first command
struct command *commandn = NULL; // A pointer to the last command

struct process *ready1 = NULL; // A pointer to the first ready process
struct process *readyn = NULL; // A pointer to the last ready process

struct sleeping *sleeping1 = NULL; // A pointer to the first sleeping process

struct bus bus = {NULL, 0}; // A struct to represent the bus

struct process *waiting_with_no_children = NULL; // A pointer to the only waiting process with no children

int num_processes_waiting_for_IO = 0; // An int to store the number of processes waiting for IO

int system_time = 0; // An int to store the system time
int cpu_time = 0; // An int to store the cpu time

int time_quantum = DEFAULT_TIME_QUANTUM; // An int to store the time quantum

void *malloc_data(size_t size) {
    // Allocates memory for data and checks if malloc fails
    void *new_data = malloc(size); // Allocate memory for the new data
    if (new_data == NULL) {
        // If malloc fails, print an error message and exit the program
        fprintf(stderr, "Error: Failed to allocate memory for new data\n");
        exit(EXIT_FAILURE);
        return NULL;
    }
    return new_data;
}

int descending(void *a, void *b) {
    // Compares two ints and returns 1 if the first int is greater than the second int
    int *int_a = (int *) a; // Cast the first int to an int
    int *int_b = (int *) b; // Cast the second int to an int
    if (*int_a > *int_b) {
        return 1; // Return 1 if the first int is greater than the second int
    }
    return 0; // Return 0 if the first int is not greater than the second int
}

int ascending(void *a, void *b) {
    // Compares two ints and returns 1 if the first int is less than the second int
    int *int_a = (int *) a; // Cast the first int to an int
    int *int_b = (int *) b; // Cast the second int to an int
    if (*int_a < *int_b) {
        return 1; // Return 1 if the first int is less than the second int
    }
    return 0; // Return 0 if the first int is not less than the second int
}

void enqueue(void **queue_head, void **queue_tail, void *data, size_t next_attribute_offset, size_t comparison_attribute_offset, int(*comparison) (void *, void *)) {
    // Adds data into a queue based on the comparison function and attribute
    if (comparison == NULL) {
        // If the comparison function is NULL, insert the data at the end of the queue
        if (*queue_head == NULL) {
            *queue_head = data; // If the queue is empty, set the head of the queue to the data
        } else {
            *(void **)((char *)*queue_tail + next_attribute_offset) = data; // Set the next attribute of the tail of the queue to the data
        }
        *queue_tail = data; // Set the tail of the queue to the data
    } else {
        // If the comparison function is not NULL, insert the data in order
        if (*queue_head == NULL) {
            *queue_head = data; // If the queue is empty, set the head of the queue to the data
        } else {
            void *current = *queue_head; // Set the current data to the head of the queue
            void *previous = NULL; // Set the previous data to NULL
            while (current != NULL) {
                if (comparison((char *)data + comparison_attribute_offset, (char *)current + comparison_attribute_offset)) {
                    // If the comparison function returns true, insert the data before the current data
                    if (previous == NULL) {
                        // If the data is first in the queue, set it as the head of the queue
                        *(void **)((char *)data + next_attribute_offset) = *queue_head; // Set the next attribute of the data to the head of the queue
                        *queue_head = data; // Set the head of the queue to the data
                    } else {
                        // If the data is not first in the queue, insert it before the current data
                        *(void **)((char *)previous + next_attribute_offset) = data; // Set the next attribute of the previous data to the data
                        *(void **)((char *)data + next_attribute_offset) = current; // Set the next attribute of the data to the current data
                    }
                    break;
                }
                previous = current; // Set the previous data to the current data
                current = *(void **)((char *)current + next_attribute_offset); // Set the current data to the next data in the queue
            }
            if (current == NULL) {
                *(void **)((char *)previous + next_attribute_offset) = data; // If the data has the highest value, set it as the tail of the queue
            }
        }
    }
}

int create_device(char *name, int read_speed, int write_speed) {
    // Adds a new device to the linked list of devices, ordered by read speed (descending)
    struct device *new_device = (struct device *) malloc_data(sizeof(struct device)); // Allocate memory for the new device
    new_device->name = name; // Set the name of the new device
    new_device->read_speed = read_speed; // Set the read speed of the new device
    new_device->write_speed = write_speed; // Set the write speed of the new device
    new_device->queue_head = NULL; // Set the head of the queue of processes waiting to use the device to NULL
    new_device->queue_tail = NULL; // Set the tail of the queue of processes waiting to use the device to NULL
    new_device->next = NULL; // Set the next device in the linked list to NULL
    enqueue((void **)&device1, NULL, new_device, offsetof(struct device, next), offsetof(struct device, read_speed), descending); // Add the new device to the linked list of devices, ordered by read speed (descending
    return 0; // Return 0 to indicate success
}

int create_process(struct command *command, struct process *parent) {
    // Adds a new process to the end of the ready linked list
    printf("%d: Process %s created and appended to READY\n", system_time, command->name); // Print a message to indicate that the process has been created
    struct process *new_process = (struct process *) malloc_data(sizeof(struct process)); // Allocate memory for the new process
    new_process->command = command; // Set the command the new process is executing
    new_process->syscall = NULL; // Set the syscall the new process is executing to NULL
    new_process->time = 0; // Set the time of the new process to 0
    new_process->num_children = 0; // Set the number of children of the new process to 0
    new_process->waiting_bool = 0; // Set the waiting boolean of the new process to 0
    new_process->parent = parent; // Set the parent of the new process
    new_process->next = NULL; // Set the next process in the linked list to NULL
    enqueue((void **)&ready1, (void **)&readyn, new_process, offsetof(struct process, next), 0, NULL); // Add the new process to the end of the ready linked list
    return 0; // Return 0 to indicate success
}

int create_command(char *name) {
    // Adds a new command to the end of the command linked list
    struct command *new_command = (struct command *) malloc_data(sizeof(struct command)); // Allocate memory for the new command
    new_command->name = name; // Set the name of the new command
    new_command->queue_head = NULL; // Set the head of the queue of syscalls that the command needs to execute to NULL
    new_command->queue_tail = NULL; // Set the tail of the queue of syscalls that the command needs to execute to NULL
    new_command->next = NULL; // Set the next command in the linked list to NULL
    enqueue((void **)&command1, (void **)&commandn, new_command, offsetof(struct command, next), 0, NULL); // Add the new command to the end of the command linked list
    return 0; // Return 0 to indicate success
}

int create_syscall(struct command *parent_command, int time, enum syscall_types type, struct device *device, struct command *command, int data) {
    // Adds a new syscall to the end of the parent command's syscall linked list
    struct syscall *new_syscall = (struct syscall *) malloc_data(sizeof(struct syscall)); // Allocate memory for the new syscall
    new_syscall->time = time; // Set the time the syscall takes to execute
    new_syscall->type = type; // Set the type of the syscall
    new_syscall->device = device; // Set the device that the syscall needs to use
    new_syscall->command = command; // Set the command that the syscall needs to spawn
    new_syscall->data = data; // Set the data that the syscall needs to read or write or the time that the process needs to sleep for
    new_syscall->next = NULL; // Set the next syscall in the linked list to NULL
    enqueue((void **)&parent_command->queue_head, (void **)&parent_command->queue_tail, new_syscall, offsetof(struct syscall, next), offsetof(struct syscall, time), ascending); // Add the new syscall to the end of the parent command's syscall linked list
    return 0; // Return 0 to indicate success
}

int create_sleeping(struct process *process, int time) {
    // Adds a new sleeping process to the sleeping linked list in order of time (ascending)
    struct sleeping *new_sleeping = (struct sleeping *) malloc_data(sizeof(struct sleeping)); // Allocate memory for the new sleeping process
    new_sleeping->process = process; // Set the process that is sleeping
    new_sleeping->time = time + system_time; // Set the time that the process will wake up
    new_sleeping->next = NULL; // Set the next sleeping process in the linked list to NULL
    enqueue((void **)&sleeping1, NULL, new_sleeping, offsetof(struct sleeping, next), offsetof(struct sleeping, time), ascending); // Add the new sleeping process to the sleeping linked list in order of time (ascending)
    return 0; // Return 0 to indicate success
}

int read_sysconfig(char argv0[], char filename[]) {
    // Reads the sysconfig file and creates the devices and sets the time quantum
    FILE *file = fopen(filename, "r"); // Open the sysconfig file
    if (file == NULL) {
        fprintf(stderr, "Error: %s failed to open file %s\n", argv0, filename); // Print an error message if the file cannot be opened
        exit(EXIT_FAILURE); // Exit the program
        return 1; // Return 1 to indicate failure
    }
    char line[100]; // A string to store each line of the file
    while (fgets(line, sizeof(line), file) != NULL) {
        // Read each line of the file
        if (line[0] == CHAR_COMMENT || line[0] == '\n') {
            continue; // Skip comment lines and empty lines
        }
        char *type = (char *) malloc_data(12); // A string to store the type of the device
        sscanf(line, "%s", type); // Read the type from the line
        if (strcmp(type, "device") == 0) {
            // If the type is device, create a new device
            char *name = (char *) malloc_data(MAX_DEVICE_NAME+1); // A string to store the name of the device
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
            fprintf(stderr, "Error: %s failed to read sysconfig file %s due to invalid type %s\n", argv0, filename, type); // Print an error message if the type is invalid
            exit(EXIT_FAILURE); // Exit the program
            return 1; // Return 1 to indicate failure
        }
    }
    fclose(file); // Close the sysconfig file
    return 0; // Return 0 to indicate success
}

int read_commands(char argv0[], char filename[]) {
    // Reads the command file and creates the commands and their syscalls
    FILE *file = fopen(filename, "r"); // Open the command file
    if (file == NULL) {
        fprintf(stderr, "Error: %s failed to open file %s\n", argv0, filename); // Print an error message if the file cannot be opened
        exit(EXIT_FAILURE); // Exit the program
        return 1; // Return 1 to indicate failure
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
            char *type = (char *) malloc_data(6); // A string to store the type of the syscall
            sscanf(line, "%dusecs %s", &time, type); // Read the time and type of the syscall from the line
            if (strcmp(type, "spawn") == 0) {
                // If the type is spawn, read the name of the command that needs to be spawned
                char *name = (char *) malloc_data(MAX_COMMAND_NAME+1); // A string to store the name of the command
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
                char *name = (char *) malloc_data(MAX_DEVICE_NAME+1); // A string to store the name of the device
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
                    fprintf(stderr, "Error: %s failed to read command file %s due to invalid device %s\n", argv0, filename, name); // Print an error message if the device does not exist
                    exit(EXIT_FAILURE); // Exit the program
                    return 1; // Return 1 to indicate failure
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
                fprintf(stderr, "Error: %s failed to read command file %s due to invalid type %s\n", argv0, filename, type); // Print an error message if the type is invalid
            }
        } else {
            // If the line does not start with a tab, create a new command
            char *name = (char *) malloc_data(MAX_COMMAND_NAME+1); // A string to store the name of the command
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
    return 0; // Return 0 to indicate success
}

//  ----------------------------------------------------------------------

int move_to_bus(void) {
    // Moves the first process on the device with the highest read speed to the bus
    if (bus.process != NULL || num_processes_waiting_for_IO == 0) {
        return 1; // Return 1 to indicate failure
    }
    struct device *device = device1; // Set device to the first device in the linked list
    while (device != NULL) {
        // Find the first device that has a process waiting to use it
        if (device->queue_head != NULL) {
            // If the device has a process waiting to use it, move the process to the bus
            bus.process = device->queue_head; // Set the process that is using the bus to the first process in the queue of processes waiting to use the device
            device->queue_head = device->queue_head->next; // Set the first process in the queue of processes waiting to use the device to the next process in the queue
            break; // Break out of the loop
        }
        device = device->next; // Set device to the next device in the linked list
    }
    if (device == NULL) {
        num_processes_waiting_for_IO = 0; // Set the number of processes waiting for IO to 0
        return 1; // Return 1 to indicate failure
    }
    num_processes_waiting_for_IO--; // Decrement the number of processes waiting for IO
    int sleep_time; // An int to store the time that the process needs to sleep until
    int speed; // An int to store the speed of the device
    struct syscall *syscall = bus.process->syscall; // Set syscall to the syscall that the process is executing (read or write)
    unsigned long long int temp_data = (unsigned long long int) syscall->data * 1000000; // Multiply the data by 1000000 so it can work with microseconds
    if (syscall->type == READ) {
        // If the syscall is read, set the speed of the device to the read speed of the device
        speed = syscall->device->read_speed;
    } else {
        // If the syscall is write, set the speed of the device to the write speed of the device
        speed = syscall->device->write_speed;
    }
    if (temp_data % speed == 0) {
        // If the data can be read or written in a whole number of microseconds, set the time that the process needs to sleep until to the data divided by the speed
        sleep_time = temp_data / speed;
    } else {
        // If the data cannot be read or written in a whole number of microseconds, set the time that the process needs to sleep until to the data divided by the speed plus 1
        sleep_time = (temp_data / speed) + 1;
    }
    sleep_time += TIME_ACQUIRE_BUS; // Add the time it takes to acquire the bus to the time that the process needs to sleep until
    if (syscall->type == READ) {
        printf("%d: Process %s is acquiring the bus and will relinquish it in %dusecs after reading %dB from device %s\n", system_time, bus.process->command->name, sleep_time, syscall->data, syscall->device->name); // Print a message to indicate that the process is acquiring the bus
    } else {
        printf("%d: Process %s is acquiring the bus and will relinquish it in %dusecs after writing %dB to device %s\n", system_time, bus.process->command->name, sleep_time, syscall->data, syscall->device->name); // Print a message to indicate that the process is acquiring the bus
    }
    bus.time = system_time + sleep_time; // Set the time that the process will finish using the bus
    return 0; // Return 0 to indicate success
}

enum transition {
    // An enum to represent the possible states that can be transitioned to
    READY,
    SLEEPING,
    WAITING,
    IO
};

int state_transition(struct process *process, enum transition transition) {
    // Transitions the process to the next state
    char *transitions[] = {"READY", "SLEEPING", "WAITING", "IO"}; // An array of strings to store the possible transitions
    system_time += TIME_CORE_STATE_TRANSITIONS; // Add the time it takes to transition states to the system time
    if (transition == READY) {
        // If the process is moving to ready, add it to the end of the ready linked list
        printf("%d-%d: Process %s state transitioned to READY\n", system_time-TIME_CORE_STATE_TRANSITIONS+1, system_time, process->command->name); // Print a message to indicate that the process has transitioned to ready
        enqueue((void **)&ready1, (void **)&readyn, process, offsetof(struct process, next), 0, NULL);
    } else if (transition == SLEEPING) {
        // If the process is moving to sleeping, add it to the sleeping linked list in order of time (ascending)
        printf("%d-%d: Process %s state transitioned to SLEEPING and will wake up in %dusecs\n", system_time-TIME_CORE_STATE_TRANSITIONS+1, system_time, process->command->name, process->syscall->data-TIME_CORE_STATE_TRANSITIONS); // Print a message to indicate that the process has transitioned to sleeping
        create_sleeping(process, process->syscall->data-TIME_CORE_STATE_TRANSITIONS);
    } else if (transition == WAITING) {
        // If the process is moving to waiting, set its waiting boolean to 1
        if (process->num_children == 0) {
            return 1; // Return 1 to indicate failure
        }
        printf("%d-%d: Process %s state transitioned to WAITING\n", system_time-TIME_CORE_STATE_TRANSITIONS+1, system_time, process->command->name); // Print a message to indicate that the process has transitioned to waiting
        process->waiting_bool = 1;
    } else {
        // If the process is moving to IO, add it to the queue of processes waiting to use the device
        printf("%d-%d: Process %s state transitioned to IO on device %s\n", system_time-TIME_CORE_STATE_TRANSITIONS+1, system_time, process->command->name, process->syscall->device->name); // Print a message to indicate that the process has transitioned to IO
        num_processes_waiting_for_IO++; // Increment the number of processes waiting for IO
        enqueue((void **)&process->syscall->device->queue_head, (void **)&process->syscall->device->queue_tail, process, offsetof(struct process, next), 0, NULL); // Add the process to the end of the queue of processes waiting to use the device
    }
    return 0; // Return 0 to indicate success
}

int move_from_sleeping(void) {
    if (sleeping1 == NULL || sleeping1->time > system_time) {
        return 1; // Return 1 to indicate failure
    }
    printf("%d: Process %s woke up\n", system_time, sleeping1->process->command->name); // Print a message to indicate that the process has woken up
    struct process *process = sleeping1->process; // Set process to the process that has woken up
    struct sleeping *temp = sleeping1->next; // Set temp to the next sleeping process in the linked list
    free(sleeping1); // Free the memory for the sleeping process
    sleeping1 = temp; // Set the first sleeping process in the linked list to temp
    state_transition(process, READY); // Transition the process to ready
    return 0; // Return 0 to indicate success
}

int run_process(void) {
    char *syscall_types[] = {"spawn", "read", "write", "sleep", "wait", "exit"}; // An array of strings to store the types of syscalls
    struct process *running = ready1; // Set running to the first process in the ready linked list
    ready1 = ready1->next; // Set the first process in the ready linked list to the next process in the ready linked list
    running->next = NULL; // Set the next process in the running linked list to NULL
    system_time += TIME_CONTEXT_SWITCH; // Add the time it takes to perform a context switch to the system time
    printf("%d-%d: Process %s context switched to RUNNING\n", system_time-TIME_CONTEXT_SWITCH+1, system_time, running->command->name); // Print a message to indicate that the process has been context switched to running
    struct syscall *syscall;
    if (running->syscall == NULL) {
        // If the process has no syscall, set syscall to the first syscall in the linked list
        syscall = running->command->queue_head;
    } else {
        // If the process has a syscall, set syscall to the next syscall in the linked list
        syscall = running->syscall->next;
    }
    int time_to_syscall = syscall->time - running->time; // An int to store the time until the process reaches its next syscall
    int temp_time = system_time; // A temporary variable to store the system time
    if (time_to_syscall+ADDITIONAL_SYSCALL_EXECUTION_TIME <= time_quantum) {
        // If the running process will reach its next syscall and be able to execute it before the end of its timeslice, execute the syscall
        cpu_time += time_to_syscall; // Add the time until the process reaches its next syscall to the CPU time
        running->time = syscall->time; // Set the time of the running process to the time of the syscall
        system_time += time_to_syscall + ADDITIONAL_SYSCALL_EXECUTION_TIME; // Set the system time to the time that the process will finish executing the syscall
        running->syscall = syscall; // Set the current syscall of the running process to the syscall
        printf("%d-%d: Process %s executed syscall %s\n", temp_time, system_time, running->command->name, syscall_types[syscall->type]); // Print a message to indicate that the process has executed the syscall
        if (syscall->type == SPAWN) {
            create_process(syscall->command, running); // Create the process
            running->num_children++; // Increment the number of children of the running process
            state_transition(running, READY); // Advance the running process to READY (append it to the end of the ready linked list)
        } else if (syscall->type == READ || syscall->type == WRITE) {
            state_transition(running, IO); // Advance the running process to IO (append it to the end of the queue of processes waiting to use the device it needs to use)
        } else if (syscall->type == SLEEP) {
            state_transition(running, SLEEPING); // Advance the running process to SLEEPING (append it to the sleeping linked list in order of wake up time)
        } else if (syscall->type == WAIT) {
            if (running->num_children == 0) {
                // If the running process has no children, advance it to READY (doesn't need to wait for any children to exit)
                printf("%d: Process %s had no children to wait for and advanced to READY\n", system_time, running->command->name); // Print a message to indicate that the process has no children to wait for
                state_transition(running, READY); // Advance the running process to READY (append it to the end of the ready linked list)
            } else {
                state_transition(running, WAITING); // Advance the running process to WAITING (set its waiting boolean to 1 and leave it hanging in memory)
            }
        } else {
            if (running->parent != NULL) {
                running->parent->num_children--; // Decrement the number of children of the parent of the running process
                if (running->parent->num_children == 0 && running->parent->waiting_bool == 1) {
                    waiting_with_no_children = running->parent; // Set the waiting process to the parent of the running process
                }
            }
            free(running); // Free the memory for the running process
        }
    } else {
        cpu_time += time_quantum; // Add the time quantum to the CPU time
        running->time += time_quantum; // Add the time quantum to the time of the running process
        system_time += time_quantum; // Add the time quantum to the system time
        printf("%d-%d: Process %s exhausted its timeslice\n", temp_time, system_time, running->command->name); // Print a message to indicate that the process has finished its timeslice
        state_transition(running, READY); // Transition the process to ready
    }
    return 0; // Return 0 to indicate success
}

int execute_commands(void) {
    create_process(command1, NULL); // Create the first process
    int simulating = 1; // An int to store whether the simulation is running
    int temp_time; // A temporary variable to store the system time
    while (simulating) {
        temp_time = system_time; // Store the system time at the start of this iteration of the simulation
        simulating = 0; // Default simulating to 0
        while (sleeping1 != NULL && sleeping1->time <= temp_time) {
            // Unblock any sleeping processes that have finished sleeping (at the time this simulation iteration started)
            simulating = 1; // An action was simulated and time was advanced
            move_from_sleeping(); // Unblock the first sleeping process in the linked list
        }
        if (waiting_with_no_children != NULL) {
            // Unblock the waiting process with no children (there will only be one with no children as this only occurs if the process that just ran exited)
            simulating = 1; // An action was simulated and time was advanced
            printf("%d: Process %s had no children left to wait for and advanced to READY\n", system_time, waiting_with_no_children->command->name); // Print a message to indicate that the process has no children to wait for
            waiting_with_no_children->waiting_bool = 0; // Set the waiting boolean of the waiting process to 0
            state_transition(waiting_with_no_children, READY); // Advance the waiting process to READY (append it to the end of the ready linked list)
            waiting_with_no_children = NULL; // Set the waiting process to NULL
        } 
        if (bus.process != NULL && bus.time <= temp_time) {
            // Unblock the process on the bus if it has finished using the bus
            simulating = 1; // An action was simulated and time was advanced
            printf("%d: Process %s relinquished the bus\n", system_time, bus.process->command->name); // Print a message to indicate that the process has relinquished the bus
            state_transition(bus.process, READY); // Advance the process to READY (append it to the end of the ready linked list)
            bus.process = NULL; // Set the process using the bus to NULL
        }
        if (bus.process == NULL && num_processes_waiting_for_IO != 0) {
            // Commence any pending IO if the bus is free
            simulating = 1; // An action was simulated and time was advanced
            move_to_bus(); // Move the first process on the device with the highest read speed to the bus
        } 
        if (ready1 != NULL) {
            // Commence/resume the next READY process
            simulating = 1; // An action was simulated and time was advanced
            run_process(); // Run the next process in the ready linked list
        }
        if (!simulating) {
            // If nothing occurred in this iteration of the simulation, time jump to the next event (if there isn't one, the simulation will end)
            if (sleeping1 != NULL) {
                simulating = 1; // An action was simulated and time was advanced
                if (bus.process != NULL) {
                    // If there is a process on the bus, set the system time to the time that the process on the bus will finish using the bus or the time that the first sleeping process will wake up (whichever is sooner)
                    if (sleeping1->time < bus.time) {
                        system_time = sleeping1->time;
                    } else {
                        system_time = bus.time;
                    }
                } else {
                    // If there is no process on the bus, set the system time to the time that the first sleeping process will wake up
                    system_time = sleeping1->time;
                }
            } else if (bus.process != NULL) {
                // If there is a process on the bus, set the system time to the time that the process on the bus will finish using the bus
                simulating = 1; // An action was simulated and time was advanced
                system_time = bus.time;
            }
        }
    }
    return 0; // Return 0 to indicate success
}

//  ----------------------------------------------------------------------

int debugging(void) {
    // A function to print the devices, commands and syscalls
    char *syscall_types[] = {"spawn", "read", "write", "sleep", "wait", "exit"}; // An array of strings to store the types of syscalls
    struct device *device = device1; // Set device to the first device in the linked list
    while (device != NULL) {
        printf("Device: %s, Read Speed: %dBps, Write Speed: %dBps\n", device->name, device->read_speed, device->write_speed); // Print the name, read speed and write speed of the device
        device = device->next; // Set device to the next device in the linked list
    }
    struct command *command = command1; // Set command to the first command in the linked list
    while (command != NULL) {
        printf("Command: %s\n", command->name); // Print the name of the command
        struct syscall *syscall = command->queue_head; // Set syscall to the first syscall in the linked list
        while (syscall != NULL) {
            if (syscall->type == SPAWN) {
                printf("Time: %d, Type: %s, Command: %s\n", syscall->time, syscall_types[syscall->type], syscall->command->name); // Print the time, type and name of the command that the syscall needs to spawn
            } else if (syscall->type == READ || syscall->type == WRITE) {
                printf("Time: %d, Type: %s, Device: %s, Data: %d\n", syscall->time, syscall_types[syscall->type], syscall->device->name, syscall->data); // Print the time, type, name of the device that the syscall needs to read from or write to and the data that the syscall needs to read or write
            } else if (syscall->type == SLEEP) {
                printf("Time: %d, Type: %s, Sleep Time: %d\n", syscall->time, syscall_types[syscall->type], syscall->data); // Print the time, type and time that the process needs to sleep for
            } else {
                printf("Time: %d, Type: %s\n", syscall->time, syscall_types[syscall->type]); // Print the time and type of the syscall
            }
            syscall = syscall->next; // Set syscall to the next syscall in the linked list
        }
        command = command->next; // Set command to the next command in the linked list
    }
    printf("Time Quantum: %d\n", time_quantum); // Print the time quantum
    return 0; // Return 0 to indicate success
}

int free_memory(void) {
    // A function to free the memory for the devices, commands and syscalls
    struct device *device = device1; // Set device to the first device in the linked list
    while (device != NULL) {
        // Free the memory for the devices
        struct device *temp = device->next;
        free(device);
        device = temp;
    }
    struct command *command = command1; // Set command to the first command in the linked list
    while (command != NULL) {
        struct syscall *syscall = command->queue_head; // Set syscall to the first syscall in the linked list
        while (syscall != NULL) {
            // Free the memory for the syscalls
            struct syscall *temp = syscall->next;
            free(syscall);
            syscall = temp;
        }
        // Free the memory for the commands
        struct command *temp = command->next;
        free(command);
        command = temp;
    }
    return 0; // Return 0 to indicate success
}

int main(int argc, char *argv[]) {
    // ENSURE THAT WE HAVE THE CORRECT NUMBER OF COMMAND-LINE ARGUMENTS
    if(argc != 3) {
        fprintf(stderr, "Usage: %s sysconfig-file command-file\n", argv[0]); // Print an error message if the number of command-line arguments is incorrect
        exit(EXIT_FAILURE); // EXIT WITH A NON-ZERO EXIT CODE TO INDICATE AN ERROR
        return 1; // Return 1 to indicate an error
    }

    // READ THE SYSTEM CONFIGURATION FILE
    read_sysconfig(argv[0], argv[1]);
    
    // READ THE COMMAND FILE
    read_commands(argv[0], argv[2]);
    
    // DEBUGGING
    debugging();

    // EXECUTE COMMANDS, STARTING AT FIRST IN command-file, UNTIL NONE REMAIN
    execute_commands();

    double cpu_utilisation = (double) cpu_time / system_time * 100; // Calculate the CPU utilisation
    
    // PRINT THE PROGRAM'S RESULTS
    printf("measurements  %i  %i\n", system_time, (int) cpu_utilisation);
    
    // Free the memory for the syscalls, commands and devices
    free_memory();

    exit(EXIT_SUCCESS); // EXIT WITH A SUCCESSFUL RETURN CODE
    return 0; // Return 0 to indicate success
}