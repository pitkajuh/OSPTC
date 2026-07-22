module file_type
  use section_type
  use interpolate
  implicit none

  type, abstract :: file
     integer :: n_ionization
   contains
     procedure(create_file), deferred :: create
     procedure(file_destructor), deferred :: file_clear
  end type file

  abstract interface
     subroutine create_file(this, z, ios, n)
       import file
       class(file), intent(inout) :: this
       integer :: z, ios, n
     end subroutine create_file

     subroutine file_destructor(this)
       import file
       class(file), intent(inout):: this
       integer :: i
     end subroutine file_destructor
  end interface

  type, extends(file) :: MF23
     type(section_coherent_scattering) :: coherent_scattering
     type(section_incoherent_scattering) :: incoherent_scattering
     type(section_pair_formation) :: pair_formation_elec
     type(section_pair_formation) :: pair_formation_nuc
     type(section_photo_ionization), allocatable :: photo_ionization(:)
   contains
     procedure, pass :: create => create_mf23
     procedure, pass :: file_clear => clear_mf23
  end type MF23

  type, extends(file) :: MF27
     type(section_coherent_factor) :: coherent_factor
     type(section_incoherent_function) :: incoherent_function
     type(section_imaginary_factor) :: imaginary_factor
     type(section_real_factor) :: real_factor
   contains
     procedure, pass :: create => create_mf27
     procedure, pass :: file_clear => clear_mf27
  end type MF27

contains

  subroutine clear_mf27(this)
    class(MF27), intent(inout) :: this
    call section_destructor(this%coherent_factor)
    call section_destructor(this%incoherent_function)
    call section_destructor(this%imaginary_factor)
    call section_destructor(this%real_factor)
  end subroutine clear_mf27

  subroutine clear_mf23(this)
    class(MF23), intent(inout) :: this
    integer :: i
    call section_destructor(this%coherent_scattering)
    call section_destructor(this%incoherent_scattering)
    call section_destructor(this%pair_formation_elec)
    call section_destructor(this%pair_formation_nuc)

    do i=1, this%n_ionization!this%photo_ionization(n-8)
       call section_destructor(this%photo_ionization(i))
    end do
    deallocate(this%photo_ionization)
  end subroutine clear_mf23

  subroutine create_mf23(this, z, ios, n)
    class(MF23), intent(inout) :: this
    integer :: z, ios, MF, MT, n, i

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
    ! allocate(this%photo_ionization(n-8))
    allocate(this%photo_ionization(n-7))
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
    ! print *, "from", index_from-1, index_to
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
       ! print *, x_star, y_km1+((x_star-x_km1)/(x_k-x_km1))*(y_k-y_km1)
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

  subroutine create_mf27(this, z, ios, n)
    class(MF27), intent(inout) :: this
    integer :: z, ios, MF, MT, n
    n=0
    call this%coherent_factor%read_section_header(z, ios, MF, MT)
    allocate(this%coherent_factor%records(2, 2*int( &
         this%coherent_factor%header(1, 3))))

    call this%coherent_factor%read_section(z, ios, MF, MT, &
         int(this%coherent_factor%header(1, 3)), &
         this%coherent_factor%records)

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
