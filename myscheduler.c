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
    struct syscall *syscall; // A pointer to the syscall that the process is executing
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

void *malloc_data(size_t size) {
    // Allocates memory for data and checks if malloc fails
    void *new_data = malloc(size); // Allocate memory for the new data
    if (new_data == NULL) {
        // If malloc fails, print an error message and exit the program
        fprintf(stderr, "Error: Failed to allocate memory for new data\n");
        exit(EXIT_FAILURE);
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

int create_device(char *name, int read_speed, int write_speed, struct device **device1) {
    // Adds a new device to the linked list of devices, ordered by read speed (descending)
    struct device *new_device = (struct device *) malloc_data(sizeof(struct device)); // Allocate memory for the new device
    new_device->name = name; // Set the name of the new device
    new_device->read_speed = read_speed; // Set the read speed of the new device
    new_device->write_speed = write_speed; // Set the write speed of the new device
    new_device->queue_head = NULL; // Set the head of the queue of processes waiting to use the device to NULL
    new_device->queue_tail = NULL; // Set the tail of the queue of processes waiting to use the device to NULL
    new_device->next = NULL; // Set the next device in the linked list to NULL
    enqueue((void **)device1, NULL, new_device, offsetof(struct device, next), offsetof(struct device, read_speed), descending); // Add the new device to the linked list of devices, ordered by read speed (descending
    return 0; // Return 0 to indicate success
}

int create_process(struct command *command, struct process *parent, struct process **ready1, struct process **readyn, int *system_time) {
    // Adds a new process to the end of the ready linked list
    printf("Process %s created and appended to READY at time %d\n", command->name, *system_time); // Print a message to indicate that the process has been created and appended to the ready queue
    struct process *new_process = (struct process *) malloc_data(sizeof(struct process)); // Allocate memory for the new process
    new_process->command = command; // Set the command the new process is executing
    new_process->syscall = NULL; // Set the syscall the new process is executing to NULL
    new_process->time = 0; // Set the time of the new process to 0
    new_process->num_children = 0; // Set the number of children of the new process to 0
    new_process->waiting_bool = 0; // Set the waiting boolean of the new process to 0
    new_process->parent = parent; // Set the parent of the new process
    new_process->next = NULL; // Set the next process in the linked list to NULL
    enqueue((void **)ready1, (void **)readyn, new_process, offsetof(struct process, next), 0, NULL); // Add the new process to the end of the ready linked list
    return 0; // Return 0 to indicate success
}

int create_command(char *name, struct command **command1, struct command **commandn) {
    // Adds a new command to the end of the command linked list
    struct command *new_command = (struct command *) malloc_data(sizeof(struct command)); // Allocate memory for the new command
    new_command->name = name; // Set the name of the new command
    new_command->queue_head = NULL; // Set the head of the queue of syscalls that the command needs to execute to NULL
    new_command->queue_tail = NULL; // Set the tail of the queue of syscalls that the command needs to execute to NULL
    new_command->next = NULL; // Set the next command in the linked list to NULL
    enqueue((void **)command1, (void **)commandn, new_command, offsetof(struct command, next), 0, NULL); // Add the new command to the end of the command linked list
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

int create_sleeping(struct process *process, int time, struct sleeping **sleeping1, int *system_time) {
    // Adds a new sleeping process to the sleeping linked list in order of time (ascending)
    struct sleeping *new_sleeping = (struct sleeping *) malloc_data(sizeof(struct sleeping)); // Allocate memory for the new sleeping process
    new_sleeping->process = process; // Set the process that is sleeping
    new_sleeping->time = time + *system_time; // Set the time that the process will wake up
    new_sleeping->next = NULL; // Set the next sleeping process in the linked list to NULL
    enqueue((void **)sleeping1, NULL, new_sleeping, offsetof(struct sleeping, next), offsetof(struct sleeping, time), ascending); // Add the new sleeping process to the sleeping linked list in order of time (ascending)
    return 0; // Return 0 to indicate success
}

int read_sysconfig(char argv0[], char filename[], struct device **device1, int *time_quantum) {
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
            create_device(name, read_speed, write_speed, device1); // Create the device
        } else if (strcmp(type, "timequantum") == 0) {
            // If the type is timequantum, set the time quantum
            int time; // An int to store the time quantum
            sscanf(line, "%s %d", type, &time); // Read the time quantum from the line
            *time_quantum = time; // Set the time quantum
        } else {
            printf("Invalid sysconfig: %s\n", type); // Print an error message if the type is invalid
        }
    }
    fclose(file); // Close the sysconfig file
    return 0; // Return 0 to indicate success
}

int read_commands(char argv0[], char filename[], struct command **command1, struct command **commandn, struct device *device1) {
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
                struct command *command = *command1; // Set command to the first command in the linked list
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
                    create_command(name, command1, commandn); // Create the command
                    create_syscall(current_command, time, SPAWN, NULL, *commandn, 0); // Create the syscall
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
            char *name = (char *) malloc_data(MAX_COMMAND_NAME+1); // A string to store the name of the command
            sscanf(line, "%s", name); // Read the name of the command from the line
            // Check if the command already exists
            struct command *command = *command1; // Set command to the first command in the linked list
            while (command != NULL) {
                if (strcmp(command->name, name) == 0) {
                    current_command = command; // If the command already exists, set the current command to the command
                    break;
                }
                command = command->next; // Set command to the next command in the linked list
            }
            if (command == NULL) {
                // If the command does not exist, create the command
                create_command(name, command1, commandn); // Create the command
                current_command = *commandn; // Set the current command to the command
            }      
        }
    }
    fclose(file); // Close the command file
    return 0; // Return 0 to indicate success
}

//  ----------------------------------------------------------------------

struct system {
    // A struct to represent the system
    struct process **ready1; // A pointer to the first ready process
    struct process **readyn; // A pointer to the last ready process
    struct sleeping **sleeping1; // A pointer to the first sleeping process
    struct device *device1; // A pointer to the first device
    struct process **bus_process; // A pointer to the process that is using the bus
    int *system_time; // A pointer to the system time
};

int move_to_bus(struct system *system) {
    // Moves the first process on the device with the highest read speed to the bus
    if (*system->bus_process != NULL) {
        return 1; // Return 1 to indicate failure
    }
    struct device *device = system->device1; // Set device to the first device in the linked list
    while (device != NULL) {
        // Find the first device that has a process waiting to use it
        if (device->queue_head != NULL) {
            // If the device has a process waiting to use it, move the process to the bus
            *system->bus_process = device->queue_head; // Set the process that is using the bus to the first process in the queue of processes waiting to use the device
            device->queue_head = device->queue_head->next; // Set the first process in the queue of processes waiting to use the device to the next process in the queue
            (*system->bus_process)->next = NULL; // Set the next process of the process that is using the bus to NULL
            break; // Break out of the loop
        }
        device = device->next; // Set device to the next device in the linked list
    }
    if (device != NULL) {
        // If a process has been moved to the bus, move the process to sleeping
        int sleep_time; // An int to store the time that the process needs to sleep until
        int speed; // An int to store the speed of the device
        struct syscall *syscall = (*system->bus_process)->syscall; // Set syscall to the syscall that the process is executing (read or write)
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
        printf("Process %s moving to the bus at time %d\n", (*system->bus_process)->command->name, *system->system_time); // Print a message to indicate that the process has moved to the bus
        create_sleeping(*system->bus_process, sleep_time, system->sleeping1, system->system_time); // Create the sleeping process
    }
    return 0; // Return 0 to indicate success
}

enum transition {
    // An enum to represent the possible states that can be transitioned to
    READY,
    SLEEPING,
    WAITING,
    IO
};

int move_from_sleeping(struct system *system); // Forward declaration

int state_transition(struct process *process, enum transition transition, struct system *system) {
    // Transitions the process to the next state
    char *transitions[] = {"READY", "SLEEPING", "WAITING", "IO"}; // An array of strings to store the possible transitions
    *system->system_time += TIME_CORE_STATE_TRANSITIONS; // Add the time it takes to transition states to the system time
    printf("Process %s state transitioned to %s from time %d to time %d\n", process->command->name, transitions[transition], *system->system_time - TIME_CORE_STATE_TRANSITIONS+1, *system->system_time); // Print a message to indicate that the process has transitioned states
    if (transition == READY) {
        // If the process is moving to ready, add it to the end of the ready linked list
        enqueue((void **)system->ready1, (void **)system->readyn, process, offsetof(struct process, next), 0, NULL);
    } else if (transition == SLEEPING) {
        // If the process is moving to sleeping, add it to the sleeping linked list in order of time (ascending)
        create_sleeping(process, process->syscall->data-TIME_CORE_STATE_TRANSITIONS, system->sleeping1, system->system_time);
    } else if (transition == WAITING) {
        // If the process is moving to waiting, set the waiting boolean of the process to 1
        process->waiting_bool = 1;
    } else {
        // If the process is moving to IO, add it to the queue of processes waiting to use the device
        enqueue((void **)&process->syscall->device->queue_head, (void **)&process->syscall->device->queue_tail, process, offsetof(struct process, next), 0, NULL); // Add the process to the end of the queue of processes waiting to use the device
        if (*system->bus_process == NULL) {
            move_to_bus(system);
        }
    }
    if (*system->sleeping1 != NULL && (*system->sleeping1)->time <= *system->system_time) {
        // If the first sleeping process will wake up before the process finishes transitioning states, move the first sleeping process to ready
        move_from_sleeping(system);
    }
    return 0; // Return 0 to indicate success
}

int move_from_sleeping(struct system *system) {
    // Moves the first sleeping process to ready
    *system->system_time = (*system->sleeping1)->time; // Set the system time to the time at which the first sleeping process will wake up
    printf("Process %s woke up at time %d\n", (*system->sleeping1)->process->command->name, *system->system_time); // Print a message to indicate that the process has woken up
    struct process *process = (*system->sleeping1)->process; // Set process to the first sleeping process
    struct sleeping *temp = (*system->sleeping1)->next; // Set temp to the next sleeping process in the linked list
    free(*system->sleeping1); // Free the memory for the first sleeping process
    *system->sleeping1 = temp; // Set the first sleeping process to temp
    state_transition(process, READY, system); // Advance the process to the next state
    if (process == *system->bus_process) {
        *system->bus_process = NULL; // If the process that is using the bus has finished using the bus, set the process that is using the bus to NULL
        move_to_bus(system); // Move the first process on the device with the highest read speed to the bus
    }
}

// Clarifications: The bus can't be requested before the process on it has exited, the next ready process can't perform a context switch before the current process has finished (no preemptive scheduling)

int execute_commands(struct command *command1, struct device *device1, int time_quantum, int *system_time, int *cpu_time) {
    //Need to check current running process, the sleeping processes and the process on the data-bus (keep sleeping and data-bus in the sleeping linked list)
    struct process *running = NULL; // Set the running process to NULL
    struct sleeping *sleeping1 = NULL; // Set the first sleeping process in the sleeping linked list to NULL
    struct process *ready1 = NULL; // Set the first process in the ready linked list to NULL
    struct process *readyn = NULL; // Set the last process in the ready linked list to NULL
    struct process *bus_process = NULL; // Set the process that is using the bus to NULL
    char *syscall_types[] = {"spawn", "read", "write", "sleep", "wait", "exit"}; // An array of strings to store the types of syscalls
    struct system *system = (struct system *) malloc_data(sizeof(struct system)); // Allocate memory for the system struct
    // Set the pointers in the system struct to enable them to be easily passed to functions in a package
    system->ready1 = &ready1;
    system->readyn = &readyn;
    system->sleeping1 = &sleeping1;
    system->device1 = device1;
    system->bus_process = &bus_process;
    system->system_time = system_time;
    create_process(command1, NULL, &ready1, &readyn, system_time); // Create the first process
    // Run the simulation until there are no more processes in ready, running, waiting or sleeping (as sleeping also contains the process on the data-bus)
    while (ready1 != NULL || sleeping1 != NULL) { // Only need to check ready and sleeping as at the start of this loop there should be no running process, and the waiting queue cannot exist without processes in other queues
        // Check if there are any ready processes that can be moved to running
        if (ready1 != NULL) {
            running = ready1; // Set the running process to the first ready process
            ready1 = ready1->next; // Set the first ready process to the next ready process
            running->next = NULL; // Set the next process of the running process to NULL
            // Run the simulation until the running process is blocked, finishes its timeslice or exits (running will be set to NULL at the end of this loop)
            *system_time += TIME_CONTEXT_SWITCH; // Add the time it takes to switch context to the system time
            printf("Process %s context switched from READY to RUNNING from time %d to time %d\n", running->command->name, *system_time - TIME_CONTEXT_SWITCH+1, *system_time); // Print a message to indicate that the running process has switched context
            int timeslice_finish = *system_time + time_quantum; // The time at which the running process will finish its timeslice
            struct syscall *syscall; // A pointer to the next syscall to run for the running process
            if (running->syscall == NULL) {
                // If the running process has no current syscall, set syscall to the first syscall of the running process
                syscall = running->command->queue_head; // Set syscall to the first syscall of the running process
            } else {
                syscall = running->syscall->next; // Set syscall to the next syscall of the running process
            }
            int syscall_time = syscall->time - running->time + *system_time; // The time at which the running process will reach the next syscall
            while (running != NULL) {
                // Need to check the next awake sleeping process (includes the databus), the time quantum and the next syscall for the running process
                if (sleeping1 != NULL && sleeping1->time <= timeslice_finish && sleeping1->time <= syscall_time) {
                    // If the next awake sleeping process will wake up before the running process finishes its timeslice or reaches its next syscall, move the next awake sleeping process to ready
                    *cpu_time += sleeping1->time - *system_time; // Add the time that the CPU has been running for to the CPU time
                    running->time += sleeping1->time - *system_time; // Add the time that the CPU has been running for to the running process's time
                    move_from_sleeping(system); // Move the next awake sleeping process to ready
                } else if (syscall_time < timeslice_finish && *system_time < timeslice_finish) {
                    // If the running process will reach its next syscall before it finishes its timeslice, execute the syscall
                    *cpu_time += syscall_time - *system_time; // Add the time that the CPU has been running for to the CPU time
                    running->time = syscall->time; // Set the time of the running process to the time of the syscall
                    *system_time = syscall_time; // Set the system time to the time at which the running process will reach its next syscall
                    running->syscall = syscall; // Set the current syscall of the running process to the syscall
                    printf("Process %s executed syscall %s at time %d\n", running->command->name, syscall_types[syscall->type], *system_time); // Print a message to indicate that the running process has executed the syscall
                    if (syscall->type == SPAWN) {
                        create_process(syscall->command, running, &ready1, &readyn, system_time); // Create the process
                        running->num_children++; // Increment the number of children of the running process
                        state_transition(running, READY, system); // Advance the running process to the next state
                    } else if (syscall->type == READ || syscall->type == WRITE) {
                        state_transition(running, IO, system); // Advance the running process to the next state
                    } else if (syscall->type == SLEEP) {
                        state_transition(running, SLEEPING, system); // Advance the running process to the next state
                    } else if (syscall->type == WAIT) {
                        if (running->num_children == 0) {
                            state_transition(running, READY, system); // Advance the running process to the next state
                        } else {
                            state_transition(running, WAITING, system); // Advance the running process to the next state
                        }
                    } else {
                        if (running->parent != NULL) {
                            running->parent->num_children--; // Decrement the number of children of the parent of the running process
                            if (running->parent->num_children == 0 && running->parent->waiting_bool == 1) {
                                running->parent->waiting_bool = 0; // Set the waiting boolean of the parent of the running process to 0
                                state_transition(running->parent, READY, system); // Advance the parent of the running process to the next state
                            }
                        }
                        free(running); // Free the memory for the running process
                    }
                    running = NULL; // Set the running process to NULL
                } else {
                    // If the running process will finish its timeslice before it reaches its next syscall, move the running process to ready (if the ready queue isn't empty)
                    *cpu_time += timeslice_finish - *system_time; // Add the time that the CPU has been running for to the CPU time
                    running->time += timeslice_finish - *system_time; // Add the time that the CPU has been running for to the running process's time
                    *system_time = timeslice_finish; // Set the system time to the time at which the running process will finish its timeslice
                    printf("Process %s finished its timeslice at time %d\n", running->command->name, *system_time); // Print a message to indicate that the running process has finished its timeslice
                    if (ready1 == NULL) {
                        printf("Process %s renewed its timeslice at time %d\n", running->command->name, *system_time); // Print a message to indicate that the running process has renewed its timeslice
                        timeslice_finish = *system_time + time_quantum; // If the ready queue is empty, set the time at which the running process will finish its timeslice to the time at which the next process will finish its timeslice
                    } else {
                        state_transition(running, READY, system); // Advance the running process to the next state
                        running = NULL; // Set the running process to NULL
                    }
                }
            }
        } else {
            // Nothing can be moved out of waiting if no process is running and can exit, so have to move the next process out of sleeping
            move_from_sleeping(system);
        }
    }
    return 0; // Return 0 to indicate success
}


//  ----------------------------------------------------------------------

int debugging(struct device *device1, struct command *command1, int *time_quantum) {
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
    printf("Time Quantum: %d\n", *time_quantum); // Print the time quantum
    return 0; // Return 0 to indicate success
}

int free_memory(struct device *device1, struct command *command1) {
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

    // INITIALISE VARIABLES
    struct device *device1 = NULL; // A pointer to the first device
    struct command *command1 = NULL; // A pointer to the first command
    struct command *commandn = NULL; // A pointer to the last command
    int time_quantum = DEFAULT_TIME_QUANTUM;
    int system_time = 0; // The system time
    int cpu_time = 0; // The CPU time

    // READ THE SYSTEM CONFIGURATION FILE
    read_sysconfig(argv[0], argv[1], &device1, &time_quantum);
    
    // READ THE COMMAND FILE
    read_commands(argv[0], argv[2], &command1, &commandn, device1);
    
    // DEBUGGING
    debugging(device1, command1, &time_quantum);

    // EXECUTE COMMANDS, STARTING AT FIRST IN command-file, UNTIL NONE REMAIN
    execute_commands(command1, device1, time_quantum, &system_time, &cpu_time);

    double cpu_utilisation = (double) cpu_time / system_time * 100; // Calculate the CPU utilisation
    int rounded_cpu_utilisation = (int) (cpu_utilisation + 0.5); // Round the CPU utilisation
    
    // PRINT THE PROGRAM'S RESULTS
    printf("measurements  %i  %i\n", system_time, rounded_cpu_utilisation);
    
    // Free the memory for the syscalls, commands and devices
    free_memory(device1, command1);

    exit(EXIT_SUCCESS); // EXIT WITH A SUCCESSFUL RETURN CODE
    return 0; // Return 0 to indicate success
}