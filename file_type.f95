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

    do
       call this%photo_ionization(i-4)%read_section_header(z, ios, MF, MT)
       if(MT==0 .and. MF==0) exit
       allocate(this%photo_ionization(i-4)%records(2, 2*int(this%photo_ionization(i-4)%header(1, 3))))
       call this%photo_ionization(i-4)%read_section(z, ios, MF, MT, &
            int(this%photo_ionization(i-4)%header(1, 3)), this%photo_ionization(i-4)%records)
       i=i+1
    end do
    this%n_ionization=i-5
  end subroutine create_mf23

  subroutine extend_scattering(array, n, deltax, n1)
    real(kind(1.d0)), intent(inout), allocatable :: array(:, :)
    real(kind(1.d0)), intent(in) :: deltax
    integer, intent(in) :: n, n1
    integer :: i

    do i=n, n+n1-1
       array(1, i+1)=array(1, i)+deltax
       array(2, i+1)=array(2, i-1)+(array(2, i)-array(2, i-1))*((array(1, i+1)-array(1, i-1))/(array(1, i)-array(1, i-1)))
    end do
  end subroutine extend_scattering

  subroutine create_mf27(this, z, ios, sizes, n)
    class(MF27), intent(inout) :: this
    integer :: z, ios, MF, MT, n, n1
    integer, allocatable :: sizes(:)
    real(kind(1.d0)) :: enext, hc, energylimit, deltax, energymin
    hc=4.135667696e-15_8*299792458.0_8
    energylimit=2.5E6_8
    energymin=1E3_8
    ! energylimit=2.1E6_8
    ! energylimit=5.1E6_8
    n1=1000

    call this%coherent_factor%read_section_header(z, ios, MF, MT)
    allocate(this%coherent_factor%records(2, 2*int( &
         this%coherent_factor%header(1, 3))+n1))
    call this%coherent_factor%read_section(z, ios, MF, MT, &
         int(this%coherent_factor%header(1, 3)), &
         this%coherent_factor%records)

    ! Coherent factor has values up to x=1E9. This limit gets exceeded easily,
    ! so more values (1000) are added by extrapolating.
    deltax=int((energylimit/hc-this%coherent_factor%records(1, &
         this%coherent_factor%n))/n1)
    call extend_scattering(this%coherent_factor%records, &
         this%coherent_factor%n, deltax, n1)
    this%coherent_factor%n=this%coherent_factor%n+n1

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
