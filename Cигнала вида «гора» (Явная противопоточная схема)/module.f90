module chill
  
  use types, only: wp
  
  implicit none
  
contains
  
  elemental real(wp) function f(x) result(y)
    real(wp), intent(in) :: x
    
    if (x .lt. 0.4_wp) then
      y = 0.2_wp
    elseif (x .lt. 0.7_wp) then
      y = 0.2_wp + (x - 0.4_wp) * 0.4_wp / 0.3_wp
    else
      y = 0.2_wp
    end if
    
  end function f
  
  subroutine solver(n, u, c, h, dt, time)
    integer, intent(in) :: n
    real(wp), dimension(0:n+1), intent(inout) :: u
    real(wp), intent(in) :: c, h, dt, time
    real(wp), dimension(:), allocatable :: u_next
    real(wp) :: t, a, b
    integer :: i
    
    allocate(u_next(n))
    
    a = (c + abs(c)) * dt / (2.0_wp * h)
    b = (c - abs(c)) * dt / (2.0_wp * h)
    t = dt
    do while (t <= time)
      do i = 1, n
        u_next(i) = u(i) - a * (u(i) - u(i-1)) - b * (u(i+1) - u(i))
      end do
      u(1:n) = u_next(1:n)
      call bound_condition(n, u)
      t = t + dt
    end do
    
    deallocate(u_next)
    
  end subroutine solver
  
  subroutine init_value(n, x, u)
    integer, intent(in) :: n
    real(wp), dimension(0:n+1), intent(in) :: x
    real(wp), dimension(0:n+1), intent(out) :: u
    
    u = f(x)
    
    call bound_condition(n, u)
    
  end subroutine init_value
  
  subroutine bound_condition(n, u)
    integer, intent(in) :: n
    real(wp), dimension(0:n+1), intent(inout) :: u
    
    u(0) = u(n-1)
    u(n+1) = u(2)
    
  end subroutine bound_condition
  
  subroutine output(x, u)
    real(wp), dimension(:), intent(in) :: x, u
    integer, parameter :: io = 102
    integer :: ios, i, n
    character(len=256) :: str
    
    n = size(x, dim=1)
    
    open(unit=io, file='result.dat', iostat=ios, iomsg=str, status="replace", action="write")
    if (ios /= 0) stop trim(str)
    do i = 1, n
      write(io, *) x(i), u(i), f(x(i))
    end do
    close(io)
    
  end subroutine output
  
  subroutine reading(n, L, c, CFL, time)
    integer, intent(out) :: n
    real(wp), intent(out) :: L, c, CFL, time
    integer, parameter :: io = 101
    integer :: ios
    character(len=256) :: str
    
    open(unit=io, file='input.dat', iostat=ios, iomsg=str, status="old", action="read")
    if (ios /= 0) stop trim(str)
    read(io, *) n
    read(io, *) L
    read(io, *) c
    read(io, *) CFL
    read(io, *) time
    close(io)
    
  end subroutine reading
  
end module chill
