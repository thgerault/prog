! calcul des coefficients binomiaux
recursive function C(n,p) result(f)
    integer, intent (in) :: n
    integer :: p
    integer :: f

    if (p==0 .or. n==p) then
        f = 1
    else
        f = C(n-1,p)+C(n-1,p-1)
    endif
end function C

program binom
integer :: C
integer :: n
integer :: p

character(8)    :: arg_n
character(8)    :: arg_p

call getarg(1,arg_n)
read(arg_n,*) n

call getarg(2,arg_p)
read(arg_p,*) p

if (p<=n .and. n>=0 .and. p>=0) then
    print*, "C(",n,",",p,")=",C(n,p)
endif
end program binom