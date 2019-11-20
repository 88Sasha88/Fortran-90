program Transpose
    implicit none
    real, dimension(:), allocatable :: fillerArray, zerosArray
    real, dimension(:, :), allocatable :: inputMatrix, outputMatrix
    real :: dummyVariable
    integer :: i, j, fillerNumber, IO, N, M, Q, trimIndex, empty
    character(50) :: extraction, fileName, fileIn, fileOut

    print*, 'Please enter the name of the file you would like to transpose.  Otherwise, enter 0 for default.'
    read*, fileIn
    if (fileIn == '0') then
        fileIn = 'ti44lan.lcoef'
    end if

    !THIS IS HOW YOU DETERMINE HOW MANY ROWS ARE IN AN INPUT FILE!
    N = 0
    IO = 0
    open(3, file = fileIn, status = 'old', action = 'read')
    do
        read(3, *, iostat = IO) dummyVariable
        if (IO > 0) then ! I think this means you're reading past the total elements in the file.
            print*, "Your shit's fucked up."
            exit
        else if (IO < 0) then ! I think this means you're reading past the total rows in the file.
            exit
        else
            N = N + 1
        end if
    end do
    close(3)
    print*, 'rows = ', N

    !THIS IS HOW YOU DETERMINE HOW MANY COLUMNS ARE IN AN INPUT FILE! (YOU HAVE TO FIND ROWS FIRST.)
    open(3, file = fileIn, status = 'old', action = 'read')
    allocate(fillerArray(N), zerosArray(N))
    do i = 1, N
        zerosArray(i) = 0
    end do
    M = 0
    IO = 0
    do
        read(3, *, iostat = IO) fillerArray
        if (IO > 0) then
            print*, "Bad news."
            exit
        else if (IO < 0) then
            exit
        else
            M = M + 1
        end if
    end do
    close(3)
    if (M == N) then
        Q = (N * M) + N
        do while (empty == 0)
            empty = 1
            deallocate(fillerArray)
            open(3, file = fileIn, status = 'old', action = 'read')
            Q = Q + N
            IO = 0
            allocate(fillerArray(Q))
            do j = 1, Q
                fillerArray(j) = 0
            end do
            read(3, *, iostat = IO) fillerArray
            if (IO > 0) then
                exit
            end if
            close(3)
            do i = 1, N
                if (fillerArray(i + M - N) /= zerosArray(i)) then
                    empty = 0
                end if
            end do
        end do
        M = (Q - N) / N
    end if
    print*, 'columns = ', M

    !TRANSPOSING THE FILE!
    allocate(inputMatrix(N, M), outputMatrix(M, N))
    open(3, file = fileIn, status = 'old', action = 'read')
    rewind(3)
    IO = 0
    do i = 1, N
        read(3, *, iostat = IO) inputMatrix(i, :)
    end do
    close(3)
    do i = 1, N
        do j = 1, M
            outputMatrix(j, i) = inputMatrix(i, j)
        end do
    end do

    !THIS IS HOW YOU MAKE AN OUTPUT FILE NAME WITHOUT HAVING TO ENTER IT MANUALLY!
    trimIndex = index(fileIn, '.') - 1
    fileOut = trim(fileIn(1:trimIndex)) // trim('Transpose.txt')
    open(4, file = fileOut, status = 'unknown')
    do i = 1, M
        write(4, *) outputMatrix(i, :)
    end do
    close(4)
    print*, trim(fileIn) // ' has been transposed to file ' // trim(fileOut) // trim('.')

end program Transpose
