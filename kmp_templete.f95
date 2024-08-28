program kmp
  implicit none
  character(1000000) :: sample, words
  integer, dimension(:), allocatable :: fail
  integer, dimension(:), allocatable :: pos
  integer :: i, j, n ,m, k, num
  
  read '(a)', sample
  read '(a)', words
  
  n = len_trim(sample)
  m = len_trim(words)
  
  
  allocate(fail(m))
  
  
  !fail function setting
  fail = 0
  j = 0
  do i = 2, m
      do while (j > 0 .and. words(i:i) /= words(j+1:j+1))
        j = fail(j)
      end do
      if (words(i:i) == words(j+1:j+1)) then
          j = j + 1
          fail(i) = j
        end if
  end do
  
  
  
  
  
  !start pattern matching
  allocate(pos(n))
  num = 0
  j = 0
  k = 1
  do i = 1, n
      do while (j > 0 .and. sample(i:i) /= words(j+1:j+1))
        j = fail(j)
      end do
      if (sample(i:i) == words(j+1:j+1)) then
        j = j + 1
      end if
      
      if (j == m) then
        pos(k) = i - m + 1
        num = num + 1
        k = k + 1
        j = fail(j)
      end if
      
  end do
  
  
  print '(i0)', num
  if (num /= 0) then
    do i = 1, num - 1
      write(*,'(i0,1x)',advance='no') pos(i)
    end do
    write(*,'(i0)',advance='no') pos(num)
  end if
  
  
  
end program
