#!/bin/bash

# Define the log file
LOG_FILE="test_results.log"

# Clear the log file if it already exists
> "$LOG_FILE"

echo "Starting Fish Parser Tests..."
echo "Results will be saved to $LOG_FILE"
echo "-----------------------------------"

# Loop through all .fish files in the test directory
for file in test/*.fish; do
    echo "Testing $file..." >> "$LOG_FILE"
    
    # Run the parser. 
    # 2>&1 redirects Error messages (stderr) to the same place as standard output (stdout).
    ./ps2yacc "$file" >> "$LOG_FILE" 2>&1
    
    # Add a separator for readability
    echo -e "\n-----------------------------------\n" >> "$LOG_FILE"
done

echo "Tests complete. Check $LOG_FILE for details."