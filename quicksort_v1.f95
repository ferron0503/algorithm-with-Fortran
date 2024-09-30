module quicksort_recursive_v1
    implicit none
    
    contains
        
        recursive subroutine sort(X,left,right)
            real(16), dimension(:), intent(inout) :: X
            real(16) :: pivot, temp
            integer :: i, j, left, right, n
            
            if (left >= right) return
            
            pivot = median3(X(left), X((left + right)/2), X(right))
            i = left
            j = right
            
            do
                do while (X(i) < pivot)
                    i = i + 1
                end do
                do while (X(j) > pivot)
                    j = j -1
                end do
                if (i <= j) then
                    temp = X(i)
                    X(i) = X(j)
                    X(j) = temp
                    i = i + 1
                    j = j - 1
                end if
                if (i > j) exit
            end do
            call sort(X,left,j)
            call sort(X,i,right)
        end subroutine sort
        
        function median3(a, b, c) result(median)
            real(16), intent(in) :: a, b, c
            real(16) :: median
            if ((a < b .and. b < c) .or. (c < b .and. b < a)) then
                median = b
            else if ((b < a .and. a < c) .or. (c < a .and. a < b)) then
                median = a
            else
                median = c
            end if
        end function median3
end module quicksort_recursive_v1
