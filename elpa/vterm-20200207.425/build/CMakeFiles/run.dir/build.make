# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.16

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:


#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:


# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list


# Suppress display of executed commands.
$(VERBOSE).SILENT:


# A target that is always out of date.
cmake_force:

.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /usr/local/Cellar/cmake/3.16.3/bin/cmake

# The command to remove a file.
RM = /usr/local/Cellar/cmake/3.16.3/bin/cmake -E remove -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /Users/jonathan/projects/dotemacs/elpa/vterm-20200207.425

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /Users/jonathan/projects/dotemacs/elpa/vterm-20200207.425/build

# Utility rule file for run.

# Include the progress variables for this target.
include CMakeFiles/run.dir/progress.make

CMakeFiles/run: ../vterm-module.so
	emacs -Q -L /Users/jonathan/projects/dotemacs/elpa/vterm-20200207.425 -L /Users/jonathan/projects/dotemacs/elpa/vterm-20200207.425/build --eval \(require\ \'vterm\) --eval \(vterm\)

run: CMakeFiles/run
run: CMakeFiles/run.dir/build.make

.PHONY : run

# Rule to build all files generated by this target.
CMakeFiles/run.dir/build: run

.PHONY : CMakeFiles/run.dir/build

CMakeFiles/run.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/run.dir/cmake_clean.cmake
.PHONY : CMakeFiles/run.dir/clean

CMakeFiles/run.dir/depend:
	cd /Users/jonathan/projects/dotemacs/elpa/vterm-20200207.425/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /Users/jonathan/projects/dotemacs/elpa/vterm-20200207.425 /Users/jonathan/projects/dotemacs/elpa/vterm-20200207.425 /Users/jonathan/projects/dotemacs/elpa/vterm-20200207.425/build /Users/jonathan/projects/dotemacs/elpa/vterm-20200207.425/build /Users/jonathan/projects/dotemacs/elpa/vterm-20200207.425/build/CMakeFiles/run.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/run.dir/depend

