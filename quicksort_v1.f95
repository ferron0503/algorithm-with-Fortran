module quicksort_recursive_v1
    implicit none
    
    contains
        
        recursive subroutine sort(X,left,right)
            real(16), dimension(:), intent(inout) :: X
            real(16) :: pivot, temp
            integer :: i, j, left, right, n
            
            if (left >= right) return
            
            pivot = X((left + right)/2)
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
end module quicksort_recursive_v1
