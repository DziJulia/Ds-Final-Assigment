// Distributed Systems - 2023
// Assignment 3
// Julia Dobrovodska, 3061278

#include <iostream>
#include <fstream>
#include <mpi.h>
#include <string>

//Global variables
int world_rank;
int world_size;
int num_blocks;
int matrix[2][2];

// allocate memory for the rows
int inverseMatrix[2][2];

const char ALPHABET_TABLE[] = {
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',
    'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',
    'U', 'V', 'W', 'X', 'Y', 'Z'
};

//HELPERS METHODS

/**
 * Method for print an array to the console
 * using it with generic data as need to print char
 * or int
 * @param {T[]} data
 * @param {int} size
 * @return {void}
 */
template <typename T>
void printArray(T* data, int size) {
    std::cout << "[";
    for (int i = 0; i < size; i++) {
        std::cout << data[i];
        if (i < size - 1) {
            std::cout << ", ";
        }
    }
    std::cout << "]" << std::endl;
}

/**
 * Method for print an 2D  array to the console
 * to see results.
 * @param {Array[Integer]} array
 * @param {Integer} numRows
 * @param {Integer} numCols
 * @return {void}
 */
void print2DArray(int array[2][2], int numRows, int numCols) {
    for (int i = 0; i < numRows; i++) {
        for (int j = 0; j < numCols; j++) {
            printf("%d ", array[i][j]);
        }
        printf("\n"); // print a newline after each row
    }
}

/**
 * This method will multiply a block of two integers by an inverse matrix, mod
 * the result by 26 and return a decoded block of ints. Ensure your modulus
 * returns the correct mathematical result for both positive and negative numbers.
 * @param {Array[Integer]} block
 * @return {Array[Integer]} inverseMatrix
 */
int* decodeBlock(int block[2], int inverseMatrix[2][2]) {
    int* decodedBlock = new int[2];
    // Matrix multiplication
    for (int i = 0; i < 2; i++) {
        for (int j = 0; j < 2; j++) {
            decodedBlock[i] += block[j] * inverseMatrix[i][j];
        }
        decodedBlock[i] = (decodedBlock[i] % 26 + 26) % 26;
    }
    return decodedBlock;
}

/**
 * Ensure the inverse is correctly calculated by completing check
 * A == A^-1
 * @param {Array[Integer,Integer]} n
 * @param {Array[Integer,Integer]} n
 * @return {Boolean}
 */
bool isInverse(int a[2][2], int b[2][2]) {
    int identity[2][2] = { {1, 0}, {0, 1} };
    int product[2][2];

    // Calculate product of a and b
    product[0][0] = (a[0][0] * b[0][0] + a[0][1] * b[1][0]) % 26;
    product[0][1] = (a[0][0] * b[0][1] + a[0][1] * b[1][1]) % 26;
    product[1][0] = (a[1][0] * b[0][0] + a[1][1] * b[1][0]) % 26;
    product[1][1] = (a[1][0] * b[0][1] + a[1][1] * b[1][1]) % 26;

    // Check if product is the identity matrix
    for (int i = 0; i < 2; i++) {
        for (int j = 0; j < 2; j++) {
            if (product[i][j] != identity[i][j]) {
                return false;
            }
        }
    }
    return true;
}

/**
 * This is helper method which check if the number inputed
 * is in range of 0 - 25
 * @param {Integer} n
 * @return {Integer}
 */
int checkValue(int n) {
    // while is smaller than 0 we adding 26
    while (n < 0) {
        n += 26 ;
    }
    // if it is bigger than 25 than we do mod on nubmer
    if (n > 26)
    {
        n = n % 26;
    }

    return n;
}

/**
 * This will calculate the determinant of a 2x2 matrix
 * and return its mod 26 inverse. (use the table given in brief)
 * @param {Array[Integer]} m
 * @return {Integer}
 */
int calculateInvDeterminant(int m[2][2]) {
    int k = checkValue(m[0][0]) * checkValue( m[1][1]);
    int j = checkValue(m[0][1]) * checkValue( m[1][0]);

    int det = (k - j) % 26;
    // Making sure that our determinant is always positive nubmer
        if (det < 0) {
        det += 26;
    }

    // Find the modular multiplicative inverse of the determinant
    // if the modulo is not equal one we return an error we return
    // detereminant only if its modulo equal to 1.
    int inverse_of_matrix = -1;
    for (int i = 0; i < 26; i++) {
        if ((det * i) % 26 == 1) {
            inverse_of_matrix = i;
            break;
        }
    }
    // Check if the modular inverse of the determinant was found
    // if not print an error 
    if (inverse_of_matrix == -1) {
        std::cerr << "Error: The determinant is not invertible modulo n." << std::endl;
        exit(EXIT_FAILURE);
    }

    return  inverse_of_matrix;
}

/**
 * Method that will calculate the modulo 26 inverse of a 2x2 matrix.
 * @param {Array[Integer]} matrix
 * @param {Integer} invDet
 */
void calculateInverse(int matrix[2][2], int invDet, int inverse[2][2]) {
    inverse[0][0] = matrix[1][1] * invDet % 26;
    inverse[0][1] = -matrix[0][1] * invDet % 26;
    inverse[1][0] = -matrix[1][0] * invDet % 26;
    inverse[1][1] = matrix[0][0] * invDet % 26;
    
    // Making sure that we get always positive number
    for (int i = 0; i < 2; i++) {
        for (int j = 0; j < 2; j++) {
            inverse[i][j] = (inverse[i][j] % 26 + 26) % 26;
        }
    }
}

/**
 * Decode a Hill Cipher ciphertext given as an array of integers and return the corresponding
 * plaintext as a char*.
 * @param ciphertextNumbers An integer array containing the ciphertext numbers
 * @param ciphertextSize The length of the ciphertext array
 * @return The plaintext string as a char*
 */
char* decode(int* ciphertextNumbers, int ciphertextSize) {
    char* decodedWord = new char[ciphertextSize + 1];
    decodedWord[ciphertextSize] = '\0';

    for (int i = 0; i < ciphertextSize; i++) {
          decodedWord[i] = ALPHABET_TABLE[ciphertextNumbers[i]];
    }

    return decodedWord;
}

// COORDINATOR IN APPLICATION
void coordinator(int world_size) {
    // Read in matrix from user input
    printf("Please enter the 2x2 matrix values one at a time, "
        "hitting enter after each integer:\n");
    scanf("%d %d %d %d", &matrix[0][0], &matrix[0][1], &matrix[1][0], &matrix[1][1]);

    // Calculate inverse determinant
    int invDet = calculateInvDeterminant(matrix);
    // Print result
    printf("The mod 26 inverse determinant of the matrix is: %d\n", invDet);

    calculateInverse(matrix, invDet, inverseMatrix);

    //print out the inverse
    print2DArray(inverseMatrix,2,2);

    //do the check about if it is true inverse of original Matrix
    bool isInverseMatrix = isInverse(matrix, inverseMatrix);

    // Print result
    if (isInverseMatrix) {
        printf("The inverse matrix is the inverse of the original matrix.\n");
    } else {
        // if it is not we finish the processorss
        printf("The inverse matrix is not the inverse of the original matrix.\n");

        // exit the coordinator() function and return to the calling function
        return;
    }

    MPI_Bcast(
        inverseMatrix,
        4,
        MPI_INT,
        0,
        MPI_COMM_WORLD
    );
    std::cout << "Matrix entered is: " << std::endl;
    print2DArray(matrix,2,2);

    /**
     * Take the ciphertext either from console or from the command line, convert each
     * character into its corresponding number, and save as an array of integers using
     * the conversion table below. (coordinator only)
     */
    std::cout << "Enter the Hill Cipher text encoded:" << std::endl;
    std::string ciphertext;
    std::cin >> ciphertext;

    // Convert the ciphertext to uppercase as it is more readable
    for (int i = 0; i < ciphertext.length(); i++) {
        char c = ciphertext[i];
        if (std::islower(c)) {
            c = std::toupper(c);
            ciphertext[i] = c;
        }
    }

    // Convert the ciphertext to an array of integers using the conversion table
    // declared at the top
    int* ciphertextNumbers = new int[ciphertext.length()];
    int cipher_size = 0;
    for (int i = 0; i < ciphertext.length(); i++) {
        char c = ciphertext[i];
        for (int j = 0; j < 26; j++) {
            if (c == ALPHABET_TABLE[j]) {
                ciphertextNumbers[cipher_size++] = j;
                break;
            }
        }
    }
    //Printing array of numbers to check
    //printArray(ciphertextNumbers, cipher_size);

    /**
     * Split the array of integers into blocks of size 2, and distribute among
     * all nodes. (coordinator only)
     */
    // Split the ciphertext into blocks of size 2
    num_blocks = cipher_size / 2;
    int blocksToSend[num_blocks * 2];

    MPI_Bcast(
        &num_blocks,
        1,
        MPI_INT,
        0,
        MPI_COMM_WORLD
    );

    // Split ciphertextNumbers into blocks of 2
    for (int i = 0; i < num_blocks; i++) {
        blocksToSend[i*2] = ciphertextNumbers[i*2];
        blocksToSend[i*2+1] = (i*2+1 < cipher_size) ? ciphertextNumbers[i*2+1] : 0;
    }
    int num_blocks_per_proc = num_blocks / world_size;
    int localBlocks[num_blocks_per_proc * 2];

    // Scatter blocksToSend
    MPI_Scatter(
        blocksToSend,
        2,
        MPI_INT,
        localBlocks,
        2,
        MPI_INT,
        0,
        MPI_COMM_WORLD
    );

   //Each node will take its block and decode it using the helper method decodeBlock().
   int* decodedBlock = decodeBlock(blocksToSend,inverseMatrix);
   int* decipheredArray  = new int[cipher_size];

    /**
     * The coordinator will then collect all of the decrypted blocks and collate them
     * into a single message. The message is then converted from integer numbers into
     * characters as per the conversion table below.
    */
   MPI_Gather(
        decodedBlock,
        2,
        MPI_INT,
        decipheredArray,
        2,
        MPI_INT,
        0,
        MPI_COMM_WORLD
    );

    // printArray(decipheredArray, cipher_size);
    // printArray(decode(decipheredArray,cipher_size),cipher_size);
    //converting array to string
    std::string str(decode(decipheredArray,cipher_size),cipher_size);

    //Finally, the coordinator will output the deciphered text to console.
    std::cout << "Encoded word is: " << std::endl;
    std::cout << str << std::endl;

   // Free the memory allocated for results
   delete[] decodedBlock;
   delete[] decipheredArray;
   delete[] ciphertextNumbers;
}

// PARTICIPANT IN APPLICATION
void participant(int rank, int size) {
    // Allocate memory for matrix
    int num_blocks_per_proc = num_blocks / size;
    int localBlocks[num_blocks_per_proc * 2];

    // Broadcast inverseMatrix to all processes

    MPI_Bcast(
        inverseMatrix,
        4,
        MPI_INT,
        0,
        MPI_COMM_WORLD
    );

    MPI_Bcast(
        &num_blocks,
        1,
        MPI_INT,
        0,
        MPI_COMM_WORLD
    );

    // Scatter blocksToSend
    int blocksToSend[2];

    MPI_Scatter(
        blocksToSend,
        2,
        MPI_INT,
        localBlocks,
        2,
        MPI_INT,
        0,
        MPI_COMM_WORLD
    );

    //printf("Participant %d\n", rank);

    //Each node will take its block and decode it using the helper method decodeBlock().
    int* decodedBlock = decodeBlock(localBlocks,inverseMatrix);
    //printArray(results, 2);

    int* decipheredArray = new int [2 * num_blocks_per_proc];

    /**
     * The coordinator will then collect all of the decrypted blocks and collate them
     * into a single message. The message is then converted from integer numbers into
     * characters as per the conversion table below.
    */
    MPI_Gather(
        decodedBlock,
        2,
        MPI_INT,
        decipheredArray,
        2,
        MPI_INT,
        0,
        MPI_COMM_WORLD
    );

    // Free the memory allocated for results
    delete[] decodedBlock;
    delete[] decipheredArray;
}

int main(int argc, char** argv) {

    // Initialize MPI3
    MPI_Init(NULL,NULL);

    // Determine the world rank and size
    MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &world_size);

    // Coordinator
    if (world_rank == 0) {
        //printing out my name and student number
        std::cout << "Julia Dobrovodska, 3061278" << std::endl;
        coordinator(world_size);
    }
    else {
        // Call participant function with appropriate arguments
        participant(world_rank, world_size);
    }

    // Free memory
    // Don't need to delete inverseMatrix at all. It will be automatically deallocated
    // when it goes out of scope at the end of the function.
    // delete[][] inverseMatrix;

    // Finalize MPI
    MPI_Finalize();

    return 0;
}
