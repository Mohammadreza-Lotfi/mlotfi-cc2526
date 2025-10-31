program verlet3D
    implicit none

    integer :: k, nsteps
    real :: tau, m
    real :: fx, fy, fz
    real :: x, y, z
    real :: vx, vy, vz
    real :: fx_new, fy_new, fz_new
    real :: t

    tau = 0.2
    m = 1.0
    nsteps = 10


    fx = 0.0
    fy = 0.1
    fz = 0.0

    x = 0.0
    y = 0.0
    z = 0.0
    vx = 0.0
    vy = 0.0
    vz = 0.0

    print *, " Time(s)   X(m)      Y(m)      Z(m)"
    print *, "-----------------------------------"

    do k = 0, nsteps
        t = k * tau
        print '(F6.2, 3X, F8.4, 3X, F8.4, 3X, F8.4)', t, x, y, z

        x = x + tau*vx + 0.5*(tau**2)*fx/m
        y = y + tau*vy + 0.5*(tau**2)*fy/m
        z = z + tau*vz + 0.5*(tau**2)*fz/m

        fx_new = fx
        fy_new = fy
        fz_new = fz

        vx = vx + 0.5*tau*(fx + fx_new)/m
        vy = vy + 0.5*tau*(fy + fy_new)/m
        vz = vz + 0.5*tau*(fz + fz_new)/m
    end do

end program verlet3D

