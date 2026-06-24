module file_type
  use section_type
  use interpolate
  implicit none

  type, abstract :: file
     integer :: n_ionization
   contains
     procedure(create_file), deferred :: create
  end type file

  abstract interface
     subroutine create_file(this, z, ios, sizes, n)
       import file
       class(file), intent(inout) :: this
       integer :: z, ios, n
       integer, allocatable :: sizes(:)
     end subroutine create_file
  end interface

  type, extends(file) :: MF23
     type(section_coherent_scattering) :: coherent_scattering
     type(section_incoherent_scattering) :: incoherent_scattering
     type(section_pair_formation) :: pair_formation_elec
     type(section_pair_formation) :: pair_formation_nuc
     type(section_photo_ionization), allocatable :: photo_ionization(:)
   contains
     procedure, pass :: create => create_mf23
  end type MF23

  type, extends(file) :: MF27
     type(section_coherent_factor) :: coherent_factor
     type(section_incoherent_function) :: incoherent_function
     type(section_imaginary_factor) :: imaginary_factor
     type(section_real_factor) :: real_factor
   contains
     procedure, pass :: create => create_mf27
  end type MF27

contains

  subroutine create_mf23(this, z, ios, sizes, n)
    class(MF23), intent(inout) :: this
    integer :: z, ios, MF, MT, n, i
    integer, allocatable :: sizes(:)

    ! Skip 23501
    call this%coherent_scattering%skip_section(z, ios)
    call this%coherent_scattering%read_section_header(z, ios, MF, MT)
    allocate(this%coherent_scattering%records(2, 2*int(this%coherent_scattering%header(1, 3))))
    call this%coherent_scattering%read_section(z, ios, MF, MT, &
         int(this%coherent_scattering%header(1, 3)), this%coherent_scattering%records)

    call this%incoherent_scattering%read_section_header(z, ios, MF, MT)
    allocate(this%incoherent_scattering%records(2, 2*int(this%incoherent_scattering%header(1, 3))))
    call this%incoherent_scattering%read_section(z, ios, MF, MT, &
         int(this%incoherent_scattering%header(1, 3)), this%incoherent_scattering%records)

    call this%pair_formation_elec%read_section_header(z, ios, MF, MT)
    allocate(this%pair_formation_elec%records(2, 2*int(this%pair_formation_elec%header(1, 3))))
    call this%pair_formation_elec%read_section(z, ios, MF, MT, &
         int(this%pair_formation_elec%header(1, 3)), this%pair_formation_elec%records)

    ! Skip 23516
    call this%pair_formation_nuc%skip_section(z, ios)

    call this%pair_formation_nuc%read_section_header(z, ios, MF, MT)
    allocate(this%pair_formation_nuc%records(2, 2*int(this%pair_formation_nuc%header(1, 3))))
    call this%pair_formation_nuc%read_section(z, ios, MF, MT, &
         int(this%pair_formation_nuc%header(1, 3)), this%pair_formation_nuc%records)

    ! Skip 23522
    allocate(this%photo_ionization(n-8))
    call this%photo_ionization(1)%skip_section(z, ios)

    i=5
    ! i=4



    do
       call this%photo_ionization(i-4)%read_section_header(z, ios, MF, MT)
       if(MT==0 .and. MF==0) exit
       allocate(this%photo_ionization(i-4)%records(2, 2*int(this%photo_ionization(i-4)%header(1, 3))))



       call this%photo_ionization(i-4)%read_section(z, ios, MF, MT, &
            int(this%photo_ionization(i-4)%header(1, 3)), this%photo_ionization(i-4)%records)
       i=i+1
    end do
    ! print *, i
    this%n_ionization=i-5

  end subroutine create_mf23

  subroutine extend_scattering(array, index_from, index_to)
    real(kind(1.d0)), intent(inout), allocatable :: array(:, :)
    ! real(kind(1.d0)), intent(in) :: deltax
    integer, intent(in) :: index_from, index_to
    real(kind(1.d0)) y_km1, y_k, x_km1, x_k, x_star
    integer :: i, j
    ! print *, deltax, array(1, index_from), array(2, index_from)
    j=1
    print *, "from", index_from-1, index_to
    ! do i=index_from, index_from+index_to-1
    do i=index_from-1, index_to
       y_km1=array(2, i-1)
       x_km1=array(1, i-1)

       x_k=array(1, i)
       y_k=array(2, i)

       ! array(1, i+1)=array(1, i)!+deltax
       x_star=array(1, i+1)
       ! x_star=array(1, i)+0.05*array(1, i)
       array(1, i+1)=x_star

       ! if(j==100) then


      array(2, i+1)=y_km1+((x_star-x_km1)/(x_k-x_km1))*(y_k-y_km1)


       ! print *, array(2, i+1), (x_star-x_km1), (x_k-x_km1), y_k, y_km1, (y_k-y_km1)
       print *, x_star, y_km1+((x_star-x_km1)/(x_k-x_km1))*(y_k-y_km1)
       !    j=1
       ! else
       !    j=j+1
       ! end if




       ! if(array(2, i+1)<0) then
       !    print *, "i", i
       !    exit
       ! end if

       ! print *, y_km1, y_k,x_km1,x_k, array(2, i+1)



       ! array(1, i+1)=array(1, i-1)+deltax
       ! array(2, i+1)=array(2, i-1)+(array(2, i)-array(2, i-1))*((array(1, i+1)-array(1, i-1))/(array(1, i)-array(1, i-1)))
    end do
  end subroutine extend_scattering

  subroutine create_mf27(this, z, ios, sizes, n)
    class(MF27), intent(inout) :: this
    integer :: z, ios, MF, MT, n
    integer, allocatable :: sizes(:)

    call this%coherent_factor%read_section_header(z, ios, MF, MT)
    allocate(this%coherent_factor%records(2, 2*int( &
         this%coherent_factor%header(1, 3))))

    call this%coherent_factor%read_section(z, ios, MF, MT, &
         int(this%coherent_factor%header(1, 3)), &
         this%coherent_factor%records)










    !! EXTEND COHERENT SCATTERING

    ! allocate(coherent_factor_temporary(2, 2*int( &
    !      this%coherent_factor%header(1, 3))))
    ! call this%coherent_factor%read_section(z, ios, MF, MT, &
    !      int(this%coherent_factor%header(1, 3)), &
    !      coherent_factor_temporary)



    ! print *, this%coherent_factor%n
    ! add_from=this%coherent_factor%n+1



    ! ! Coherent factor has values up to x=1E9. This limit gets exceeded easily,
    ! ! so more values are added by extrapolating.

    ! ! this%coherent_factor%n=this%coherent_factor%n+int(n1)
    ! print *, this%coherent_factor%n

    ! ! extend_from=int(log10(this%coherent_factor%records(1, this%coherent_factor%n)))
    ! step=1000
    ! extend_from=int(log10(coherent_factor_temporary(1, this%coherent_factor%n)))
    ! extend_to=int(log10(energylimit/hc))
    ! extend_size=(extend_to-extend_from+1)*step
    ! print *, "from", extend_from, "to", extend_to
    ! a=0
    ! print *, "size", extend_size, step

    ! do i=extend_from, extend_to+1
    ! ! do i=extend_from, extend_from
    !    xx=10**real(i, kind(1.d0))
    !    delta=xx/step
    !    print *, i, xx, delta
    !    ! print *, "i", i, xx
    !    do j=1, step

    !       ! coherent_factor_temporary(1, add_from+a)=real(xx*(0.001+j), kind(1.d0))
    !       coherent_factor_temporary(1, add_from+a)=real(xx+delta*j, kind(1.d0))
    !       ! print *, coherent_factor_temporary(1, add_from+a)
    !       a=a+1
    !    end do

    ! end do

    ! do i=1, this%coherent_factor%n+extend_size
    !    ! print *, i, coherent_factor_temporary(1, i), coherent_factor_temporary(2, i)
    !    print *, coherent_factor_temporary(1, i), coherent_factor_temporary(2, i)
    ! end do

    ! this%coherent_factor%n=this%coherent_factor%n+extend_size
    ! allocate(this%coherent_factor%records(2, this%coherent_factor%n))
    ! this%coherent_factor%records=coherent_factor_temporary
    ! deallocate(coherent_factor_temporary)
    ! print *, this%coherent_factor%n

    ! print *, this%coherent_factor%n


    ! call extend_scattering(this%coherent_factor%records, &
    !      add_from, this%coherent_factor%n)








    ! this%coherent_factor%n=this%coherent_factor%n+n1
    ! this%coherent_factor%n=this%coherent_factor%n+int(n1/1E5_8)


    ! do i=1, this%coherent_factor%n
    !    print *, this%coherent_factor%records(1, i), this%coherent_factor%records(2, i)
    ! end do



    ! do i=1, this%coherent_factor%n
    !    ! print *, this%coherent_factor%records(1, i), this%coherent_factor%records(2, i)
    !    if(this%coherent_factor%records(2, i)<0) then
    !       print *, "i", i
    !       exit
    !    end if

    ! end do


    call this%incoherent_function%read_section_header(z, ios, MF, MT)
    allocate(this%incoherent_function%records(2, 2*int( &
         this%incoherent_function%header(1, 3))))
    call this%incoherent_function%read_section(z, ios, MF, MT, &
         int(this%incoherent_function%header(1, 3)), &
         this%incoherent_function%records)

    call this%imaginary_factor%read_section_header(z, ios, MF, MT)
    allocate(this%imaginary_factor%records(2, 2*int( &
         this%imaginary_factor%header(1, 3))))
    call this%imaginary_factor%read_section(z, ios, MF, MT, &
         int(this%imaginary_factor%header(1, 3)), &
         this%imaginary_factor%records)

    call this%real_factor%read_section_header(z, ios, MF, MT)
    allocate(this%real_factor%records(2, 2*int(this%real_factor%header(1, 3))))
    call this%real_factor%read_section(z, ios, MF, MT, &
         int(this%real_factor%header(1, 3)), this%real_factor%records)

  end subroutine create_mf27
end module file_type
