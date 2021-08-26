!=- fortran-libraries
!=- © Stanislav Shirokov, 2014-2020

	module cosmology
		use global
		use GNUplot

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

         !=- cosmological constants
		real(8),parameter ::	c     = 2.99792458d10   , &	!=- cm/s

                           L_sun = 3.827d33        , &   !=- erg/s
                           M_sun = 4.83d0          , &

                           O_3   = 0.3d0				, &
                           O_7   = 0.7d0           , &
                           H70   = 7d1             , &   !=- km/s/Mpc
                           dl    = 4421.71767d0          !=- c/H_0/1d5 in GCS

         !=- cosmological variables
		real(8)           ::	z_max = 2d1				   , &   !=- redshift calculated maximally

                           H_0   = 70d0            , &	!=- km/s/Mpc
                           O_m   = 0.308d0 		   , &
                           O_w   = 1d0				   , &
                           O_k   = 0d0             , &
                           q_0   = 0.5d0

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

         !=- others variables
      real(8)           :: dz, x_max, I1

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

         !=- the redshift_distance_relation procedure
      logical           :: RDR_calculating = .true.   , &   !=- .false.
                           RDR_error       = .true.

		integer,parameter :: N_RDR_grid        = 2d3    , &   !=- graph grid
                           N_RDR_grid_mode   = 3            !=- progression power

		real(8)           :: RDR(2,N_RDR_grid) , &
                           RDR_z_max = 2d1

      integer           :: RDR_type          = 1            !=- 0 is LCDM(0.3,70) , 1 is LCDM(\O_m,H_0) , 2 is wCDM(w,\O_m,H_0,O_k)

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

      logical           :: MS_recalculating = .false.

      integer,parameter :: N_models       = 9   , &
                           N_MS           = 10  , &
                           MS_grid        = 2d2 , &
                           MS_model_count = 1d2

      character(len)    :: model_set (MS_model_count,2) = 'NaN' , model_name , MS_columns(3)      , &
                           MS_fields(30) , MS_table_name = 'models.dat', MS_model(N_MS) = 'NaN'       , &
                           MS_parameters(30)

		real(8)           :: MS_z_max = 2d1 , MS_H_0 , MS_O_v , MS_O_k , MS_w , MS_O_m_bug              , &
                           MS_Omega_w (N_MS) = NaN , MS_Omega_k(N_MS) = NaN , MS_EoS_w(N_MS) = NaN    , &
                           MS_Hubble_0(N_MS) = NaN , MS_table ( 1+3*MS_model_count , MS_grid ) = 0d0  , &

                           LumDist(N_models) = 1d0, MetrDist(N_models)= 1d0, DistMod(N_models)= 1d0   !=- z for FCM > 0.0025

      integer           :: MS_real_model_count = 0 , N_grid = 2d2 , &
                           MS_count_models = 0 , MS_count_w = 0 , MS_count_O_w = 0 , MS_count_O_k = 0 , MS_count_H_0 = 0

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- truncated=136-=1
		contains

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

         subroutine make_model_set
            real(8) Omega_m
            integer i,j,k,ii,ff

         if (model_set(1,1)=='NaN') then
            call cheking_MS_parameters

            ii=0
            do jj=1,MS_count_models

               !=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=!

               if (MS_model(jj)=='wCDM') then

                  do i=          1,MS_count_w
                     do j=       1,MS_count_O_w
                        do k=    1,MS_count_O_k
                           do kk=1,MS_count_H_0
                              ii=1+ii

                              if (MS_Omega_k(k)>0) then
                                 ff=5
                                 else
                                    ff=5
                                 endif

                              Omega_m = abs ( 1 + MS_Omega_k(k) - MS_Omega_w(j) ) !=-   the Friedmann equation

                                 !=-   write(*,*) ' the model_set format:'
                                 !=-   write(*,*) ' wCDM H_0  w  Omega_v  Omega_k'
                                 !=-   write(*,*) ' FF H_0'
                                 !=-   write(*,*) ' TL H_0'

                              model_set ( ii , 1 ) = &
                                 ' wCDM ' // trim( realtostr( MS_Hubble_0(kk) ) ) // ' ' &
                                          // trim( realtostr( MS_EoS_w(i)     ) ) // ' ' &
                                          // trim( realtostr( MS_Omega_w(j)   ) ) // ' ' &
                                          // trim( realtostr( MS_Omega_k(k)   ) )

                              model_set ( ii , 2 ) = &
                                 ' wCDM (w = '           // trim( realtostrf( MS_EoS_w(i)    ,4 ,1) ) // &
                                 ', {/Symbol W}_{DE} ='  // trim( realtostrf( MS_Omega_w(j)  ,4 ,1) ) // &
                                 ', {/Symbol W}_m ='     // trim( realtostrf( Omega_m        ,4 ,1) ) // &
                                 ', {/Symbol W}_k ='     // trim( realtostrf( MS_Omega_k(k)  ,ff,1) ) // &
                                 ', H_0 ='               // trim( realtostrf( MS_Hubble_0(kk),5 ,1) ) // ') '

                              enddo
                           enddo
                        enddo
                     enddo

               !=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=!

                  else

               !=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=!

                     do kk=1,MS_count_H_0
                        ii=1+ii
                              model_set ( ii , 1 ) = &
                                 trim(MS_model(jj)) // ' ' // trim( realtostr( MS_Hubble_0(kk) ) )

                              model_set ( ii , 2 ) = trim(MS_model(jj))  &
                                 // ', H_0 =' // trim( realtostrf( MS_Hubble_0(kk),5 ,1) ) // ') '
                        end do

               !=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=!

                  endif
                  MS_real_model_count = ii
               enddo

            else

            MS_real_model_count = count_noNaN_text(model_set(:,1))

            endif

               call compute_model_set
               call write_model_set
               call plot_model_set

               call MS_default

            end

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

         subroutine cheking_MS_parameters

               MS_count_models   = count_noNaN_text(MS_model)
               MS_count_w        = count_noNaN(MS_EoS_w)
               MS_count_O_w      = count_noNaN(MS_Omega_w)
               MS_count_O_k      = count_noNaN(MS_Omega_k)
               MS_count_H_0      = count_noNaN(MS_Hubble_0)

               if ( MS_count_models==0 ) then
                  write(*,*) '   info: cheking_MS_parameters: MS_model = NaN and has been changed to LCDM'
                  MS_model(1) = 'wCDM' ; MS_count_models = 1
                  endif

               if ( MS_count_w==0 ) then
                  write(*,*) '   info: cheking_MS_parameters: MS_EoS_w = NaN and has been changed to -1'
                  MS_EoS_w(1) = -1     ; MS_count_w = 1
                  endif

               if ( MS_count_O_w==0 ) then
                  write(*,*) '   info: cheking_MS_parameters: MS_Omega_w = NaN and has been changed to 0.7'
                  MS_Omega_w(1) = O_7  ; MS_count_O_w = 1
                  endif

               if ( MS_count_O_k==0 ) then
                  write(*,*) '   info: cheking_MS_parameters: MS_Omega_k = NaN and has been changed to 0.0'
                  MS_Omega_k(1) = 0d0  ; MS_count_O_k = 1
                  endif

               if ( MS_count_H_0==0 ) then
                  write(*,*) '   info: cheking_MS_parameters: MS_Hubble_0 = NaN and has been changed to 70 km/s/Mpc'
                  MS_Hubble_0(1) = H70 ; MS_count_H_0 = 1
                  endif

            end

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

         subroutine MS_default

            !=- for cosmology module
            model_set (:,:)   = 'NaN'
            MS_table_name     = 'models.dat'
            MS_model(:)       = 'NaN'

            MS_z_max = 2d1
            MS_Omega_w (:) = NaN ; MS_Omega_k(:) = NaN ; MS_EoS_w(:) = NaN
            MS_Hubble_0(:) = NaN ; MS_table ( : , : ) = 0d0

            LumDist(:) = 1d0 ; MetrDist(:)= 1d0 ; DistMod(:)= 1d0   !=- z for FCM > 0.0025

            MS_real_model_count = 0 ; N_grid = 2d2
            MS_count_models = 0 ; MS_count_w = 0 ; MS_count_O_w = 0 ; MS_count_O_k = 0 ; MS_count_H_0 = 0

            end subroutine MS_default

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

         subroutine compute_model_set
            integer i,j ; MS_table(:,:)=0d0

               inquire( file = MS_table_name , exist = file_exists )

         if (.not. file_exists .or. MS_recalculating ) then

               do i=1,MS_model_count

                     read(model_set(i,1),*) model_name
                  if ( model_name /= 'model' ) then

                        do j=1,MS_grid

                           if (i==1) MS_table ( 1 , j ) = MS_z_max / MS_grid**2 * j**2 !(i+0.01d0)

                              select case (model_name)

                                 case('wCDM')

                                     MS_H_0 = 0d0 ; MS_w=0d0 ; MS_O_v=0d0 ; MS_O_k=0d0
                                    read(model_set(i,1),*) model_name , MS_H_0 , MS_w , MS_O_v , MS_O_k

                                    MS_O_m_bug = 1 + MS_O_k - MS_O_v !=-   the Friedmann equation
                           !write(*,*) 'MS_compute: Omega_m = ', MS_O_m_bug
                                    MS_table ( 2+3*(i-1) , j ) =  &
                                        r_wCDM( MS_table ( 1 , j ) , MS_H_0 , MS_w , MS_O_m_bug , MS_O_k )
                                    MS_table ( 3+3*(i-1) , j ) =  &
                                        d_wCDM( MS_table ( 1 , j ) , MS_H_0 , MS_w , MS_O_m_bug , MS_O_k )
                                    MS_table ( 4+3*(i-1) , j ) =  &
                                       dm_wCDM( MS_table ( 1 , j ) , MS_H_0 , MS_w , MS_O_m_bug , MS_O_k )

                                 case('FF')
                                    read(model_set(i,1),*) model_name , MS_H_0

                                    MS_table ( 2+3*(i-1) , j ) =  &
                                        r_FCM( MS_table ( 1 , j ) , MS_H_0 )
                                    MS_table ( 3+3*(i-1) , j ) =  &
                                        d_FCM( MS_table ( 1 , j ) , MS_H_0 )
                                    MS_table ( 4+3*(i-1) , j ) =  &
                                       dm_FCM( MS_table ( 1 , j ) , MS_H_0 )

                                 case('TL')
                                    read(model_set(i,1),*) model_name , MS_H_0

                                    MS_table ( 2+3*(i-1) , j ) =  &
                                        r_TL( MS_table ( 1 , j ) , MS_H_0 )
                                    MS_table ( 3+3*(i-1) , j ) =  &
                                        d_TL( MS_table ( 1 , j ) , MS_H_0 )
                                    MS_table ( 4+3*(i-1) , j ) =  &
                                       dm_TL( MS_table ( 1 , j ) , MS_H_0 )

                                 case default
                                    if (model_name/='NaN') &
                                       write(*,*) 'MS_compute: unknown model mask: ' // trim(model_name)
                                 end select

                           enddo
                     else
                        MS_real_model_count = i-1
                        write(*,*) 'MS_compute: model_count = ', MS_real_model_count
                        exit
                     end if

                  end do
            endif
            end subroutine compute_model_set

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

         subroutine write_model_set
            integer i
            character(len) title(MS_real_model_count)

               inquire( file = MS_table_name , exist = file_exists )

         if (MS_model_count>0 .and. .not. file_exists .or. MS_recalculating ) then

            MS_columns(1) = ':metric_r'
            MS_columns(2) = ':luminosity_d_L'
            MS_columns(3) = ':distance_modulus'

            open(unit_1,file=MS_table_name,status='replace')

               line=' '
               do i=1,MS_real_model_count
                  if (i>9) line=''
                  title(i) = 'model-' // trim(adjustl(inttostr(i))) // ': ' // trim(line) // trim(model_set(i,2))
                  write(unit_1,'(A2,A200)') '# ' , title(i)
                  end do

               theformat = '' ; theformat = '(A2,A23,' // trim(adjustl(inttostr(1+3*MS_real_model_count))) // &
                  '(A10,A10))'
               write(unit_1,theformat) '# ',' ', &
                  ( title(i),' ',title(i),' ',title(i),' ',i=1,MS_real_model_count )

               theformat = '' ; theformat = '(A2,A15,A8,' // trim(adjustl(inttostr(1+3*MS_real_model_count))) // '(i3,A17))'
               write(unit_1,theformat) '# ','1:redshift',' ',( 1+i,MS_columns(fix_index(i,3)),i=1,3*MS_real_model_count )

               theformat = '' ; theformat = '(' // trim(adjustl(inttostr(1+3*MS_real_model_count))) // '(E20.8))'
               write(unit_1,theformat) MS_table(1:1+3*MS_real_model_count,:)

               close(unit_1)
               else
                  if ( MS_model_count==0 ) write(*,*) 'MS: critical error'
            endif
            end subroutine write_model_set

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

         integer function fix_index( in_index , max_border )
            integer in_index , out_index , max_border
            out_index = in_index
            do while ( out_index > max_border )
               out_index = out_index - max_border
               enddo
               fix_index = out_index
            end function fix_index

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

         subroutine plot_model_set
            character(len) logscale_interlineation , log_y , logscale_lable
            integer i,j,k
               call clear_plot_fields

               pngterm  = 'set term pngcairo enhanced font "Verdana,20" size 1400, 1050'
               epsterm  = 'set term postscript enhanced color font "Verdana,14" size 8.5, 6.3'

               GNUfields(logscale)  =  'set logscale'
               GNUfields(format_y)  =  '10^{%L}'
               GNUfields(xrange)    =  '[0.01:*]'

               GNUfields(legend)    =  'right bottom'

               GNUfields(grid)      =  'xtics ytics mxtics mytics'

               MS_fields(1) = 'r(z), [Mpc]'
               MS_fields(2) = 'd_L(z), [Mpc]'
               MS_fields(3) = '{/Symbol m}(z)'

               MS_fields(4) = 'log10 r(z)/r_{' // trim( model_set (1,2) ) // '}(z)'
               MS_fields(5) = 'log10 d_L(z)/d_{L,' // trim( model_set (1,2) ) // '}(z)'
               MS_fields(6) = '{/Symbol m}(z) - {/Symbol m}_{' // trim( model_set (1,2) ) // '}(z)'

               MS_fields(7) = 'log10 {/Symbol D}r(z)'
               MS_fields(8) = 'log10 {/Symbol D}d_L(z)'
               MS_fields(9) = '{/Symbol D}{/Symbol m}(z)'

               MS_fields(10) = 'r(z)'
               MS_fields(11) = 'd_L(z)'
               MS_fields(12) = 'mu(z)'

               MS_fields(13) = 'delta_r(z)'
               MS_fields(14) = 'delta_d_L(z)'
               MS_fields(15) = 'delta_mu(z)'

               do k=1,2    !=- Delta
                  do j=1,3 !=- r(z), d_L(z), mu(z)

                     GNUfields(mxtics)    =  '2'
                     GNUfields(mytics)    =  '2'

                     logscale_lable=''
                     if ( k==1 ) then
                        logscale_lable = ', logscale'
                        GNUfields(mxtics)    =  '5'
                        GNUfields(mytics)    =  '5'
                        endif
                        GNUfields(xlabel)    =  'set xlabel "redshift z' // trim(logscale_lable) // '"'
                        if (j==3) logscale_lable = ''

                     GNUfields(title)  = 'set title "'// trim(MS_fields(j+6*(k-1))) //' for different models (CosMod v1.0)"'

                        logscale_interlineation='-logscale'
                        log_y='log10'
                     if (k==2) logscale_interlineation = ''
                     if (k==2) GNUfields(legend)    =  'set key off'
                     if (j==3 .and. k/=2) then
                        GNUfields(logscale)  = 'set logscale x'
                        GNUfields(format_y)  = '#set format y "10^{%L}"'
                        endif
                     if (k==2 .and. j==3) log_y=''



                        if (MS_real_model_count==0) MS_real_model_count = count_noNaN_text(model_set (:,1))
                     do i=1,MS_real_model_count
                        select case (k)
                           case (1)
                              GNUfields(ylabel) = 'set ylabel "' // trim(MS_fields(j+6*(k-1))) // trim(logscale_lable) // '"'

                              GNUfields (plot1+(i-1)*2) = ', "' // trim(slashfix(MS_table_name)) // &
                                 '" u 1:' // trim(inttostr(1+j+3*(i-1))) // ' w l ls ' // trim(inttostr(i)) // ' lw 3 '

                              GNUfields (title1+(i-1)*2) = ' title "' // trim( model_set (i,2) ) // '"'
                           case(2)
                              GNUfields(yrange) =  'set yrange [*:*]'
                              !=- GNUfields(yrange) =  'set yrange [-0.5:1]'
                              !=- if (j==3) GNUfields(yrange) =  'set yrange [-0.025:0.05]'

                              GNUfields(ylabel) = 'set ylabel "' // trim(MS_fields(j+6*(k-1))) // ' = ' // &
                                 trim(MS_fields(j+3*(k-1))) // '"'

                              GNUfields (plot1+(i-1)*2) = ', "' // trim(slashfix(MS_table_name)) // &
                                 '" u 1:('// trim(log_y) //'($' // trim(inttostr(1+j+3*(i-1))) // ') - '// &
                                             trim(log_y) //'($' // &
                                 trim(inttostr(1+j)) // ')) w l ls ' // trim(inttostr(i)) // ' lw 3 '

                              GNUfields (title1+(i-1)*2) = ' title "' // trim( model_set (i,2) ) // '"'
                           end select
                        end do
                     GNUfields (plot1) = GNUfields (plot1)(2:len)

                     graph_name = trim(MS_fields(j+9+3*(k-1))) // trim(logscale_interlineation)
                        GNUfields(extention_out_figure)='#eps' ; call plot(MS_table_name)
                     graph_name = trim(MS_fields(j+9+3*(k-1))) // trim(logscale_interlineation)
                        GNUfields(extention_out_figure)='#png' ; call plot(MS_table_name)
                     enddo
                  enddo
            end subroutine plot_model_set

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

      subroutine redshift_distance_relation  !=- RDR( redshift , luminosity distance )
         if (RDR_calculating) then
            do i=1,N_RDR_grid
               z = z_max**(1d0/N_RDR_grid_mode)/N_RDR_grid * i ; z = z**N_RDR_grid_mode
               RDR(1,i) = z
               RDR(2,i) = fun_from_z_to_R(z,H_0)
               end do
               RDR_calculating = .false.
            endif
         end subroutine

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

		real(8) function fun_from_R_to_z(luminosity_distance)
			real(8) luminosity_distance ; fun_from_R_to_z = 0d0
			   call redshift_distance_relation
            do i=1,N_RDR_grid
               if ( RDR(2,i) <= luminosity_distance ) fun_from_R_to_z = RDR(1,i)
               end do
			end function

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

		real(8) function fun_from_z_to_R(redshift,H0)
			real(8) redshift,H0 ; fun_from_z_to_R = 0d0
            select case (RDR_type)
                  case(0)
                     fun_from_z_to_R = D_wCDM(redshift,H0,-1d0,O_3,0d0)    !=- LCDM(0.3,0.7,H_0)
                  case(1)
                     fun_from_z_to_R = D_wCDM(redshift,H0,-1d0,O_m,0d0)    !=- LCDM(\O_m,H_0)
                  case(2)
                     fun_from_z_to_R = D_wCDM(redshift,H0,w,O_m,O_k)       !=- wCDM()
                  case default
                     if (RDR_error) write(*,*) ' do need choose the redshift-distance type '
               end select
			end function

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

      real(8) function mag_to_Lum( visible_magnitude , luminosity_distance )
         real(8) visible_magnitude,luminosity_distance ; Mag_to_Lum = 0d0
            if (visible_magnitude .ne. 0) Mag_to_Lum = L_sun * 1d1**( 0.4d0*( M_sun - &
               vis_to_abs_mag( visible_magnitude , luminosity_distance ) ) )
         end function

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

      real(8) function Lum_to_Mag(Luminosity)
         real(8) Luminosity ; Lum_to_Mag = 0d0
            Lum_to_Mag = M_sun - 2.5d0 * log10 ( Luminosity / L_sun )
         end function

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

      real(8) function abs_to_vis_mag( absolute_magnitude , luminosity_distance )
         real(8) absolute_magnitude,luminosity_distance ; abs_to_vis_mag = 0d0
            if (absolute_magnitude .ne. 0) abs_to_vis_mag = absolute_magnitude - 5d0 + 5d0 * log10 ( luminosity_distance * 1d5 ) !=- 10 pc
         end function

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

      real(8) function vis_to_abs_mag( visible_magnitude , luminosity_distance )
         real(8) visible_magnitude,luminosity_distance ; vis_to_abs_mag = 0d0
            if (visible_magnitude .ne. 0) vis_to_abs_mag = visible_magnitude + 5d0 - 5d0 * log10 ( luminosity_distance * 1d5 ) !=- 10 pc
         end function

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

			!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=!
			!real(8) function R_LCDM(z)			; real(8) z 		; R_LCDM  = dl * IntHwCDM(z,-1d0,O_m,0d0)	;	end function
   real(8) function R_LCDM(z,H0)			      ; real(8) z,H0 			; R_LCDM  = c/H0/1d5 * IntHwCDM(z,-1d0,O_3,0d0)	;	end function
   real(8) function R_PV(z,H0)					; real(8) z,H0 			; R_PV    = c/H0/1d5 * IntHwCDM(z,-1d0,0d0,0d0)	;	end function
   real(8) function R_EdeS(z,H0)				   ; real(8) z,H0 			; R_EdeS  = c/H0/1d5 * IntHwCDM(z,-1d0,1d0,0d0) ;	end function
   real(8) function R_wCDM(z,H0,w,Om,Ok)  	; real(8) z,H0,w,Om,Ok  ; R_wCDM  = c/H0/1d5 * RzwCDM(z,w,Om,Ok)	      ;	end function
   real(8) function R_CSS(z,H0)					; real(8) z,H0 			; R_CSS   = c/H0/1d5 * z								;	end function
   real(8) function R_TL(z,H0)					; real(8) z,H0 			; R_TL    = c/H0/1d5 * log(1d0+z)		 			;	end function
   real(8) function R_FCM(z,H0)					; real(8) z,H0 			; R_FCM   = c/H0/1d5 * YW(z) 						   ;	end function
   real(8) function R_MM(z,H0)					; real(8) z,H0 			; R_MM    = c/H0/1d5 * log(1d0+z) 					;	end function
			!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=!
   real(8) function D_LCDM(z,H0)					; real(8) z,H0		 		; D_LCDM  = (1d0+z)		* R_LCDM(z,H0) 			;	end function
   real(8) function D_PV(z,H0)					; real(8) z,H0		 		; D_PV    = (1d0+z)		* R_PV(z,H0) 				;	end function
   real(8) function D_EdeS(z,H0)					; real(8) z,H0		 		; D_EdeS	 = (1d0+z) 		* R_EdeS(z,H0) 			;	end function
   real(8) function D_wCDM(z,H0,w,Om,Ok)  	; real(8) z,H0,w,Om,Ok  ; D_wCDM	 = (1d0+z) 		* R_wCDM(z,H0,w,Om,Ok)  ;	end function
   real(8) function D_CSS(z,H0)					; real(8) z,H0	 		   ; D_CSS   = (1d0+z)		* R_CSS(z,H0) 				;	end function
   real(8) function D_TL(z,H0)					; real(8) z,H0				; D_TL  	 = dsqrt(1d0+z)* R_TL(z,H0) 				;	end function
   real(8) function D_FCM(z,H0)					; real(8) z,H0				; D_FCM 	 = (1d0+z) 		* R_FCM(z,H0) 				;	end function
   real(8) function D_MM(z,H0)					; real(8) z,H0				; D_MM  	 = (1d0+z) 		* R_MM(z,H0) 				;	end function
			!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=!
   real(8) function mu(dL)						   ; real(8) dL 		   	; mu		 = 25d0+5d0*log10(dL)					   ;	end function
			!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=!
   real(8) function dm_LCDM(z,H0)				; real(8) z,H0	 		   ; dm_LCDM = mu( D_LCDM(z,H0)			)		      ;	end function
   real(8) function dm_PV(z,H0)					; real(8) z,H0		 		; dm_PV	 = mu( D_PV(z,H0)				)		      ;	end function
   real(8) function dm_EdeS(z,H0)			   ; real(8) z,H0 			; dm_EdeS = mu( D_EdeS(z,H0)			)		      ;	end function
   real(8) function dm_wCDM(z,H0,w,Om,Ok)	   ; real(8) z,H0,w,Om,Ok  ; dm_wCDM = mu( D_wCDM(z,H0,w,Om,Ok))  	      ;	end function
   real(8) function dm_CSS(z,H0)					; real(8) z,H0		 		; dm_CSS	 = mu( D_CSS(z,H0)			)		      ;	end function
   real(8) function dm_TL(z,H0)					; real(8) z,H0		 		; dm_TL	 = mu( D_TL(z,H0)				)		      ;	end function
   real(8) function dm_FCM(z,H0)					; real(8) z,H0		 		; dm_FCM	 = mu( D_FCM(z,H0)			)		      ;	end function
   real(8) function dm_MM(z,H0)					; real(8) z,H0		 		; dm_MM	 = mu( D_MM(z,H0)				)		      ;	end function
			!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=!

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

			real(8) function YW(z)
				real(8) z
					YW=0d0 ; N=1d4 ; x_max=6d0

					if (z.ne.0) then

						if (z<5) x_max=3d0		!=- x_max / N =
						if (z<2) x_max=1.5d0		!=- x_max / N =
						if (z<0.5) x_max=0.5d0	!=- x_max / N =
						if (z<0.1) x_max=0.1d0	!=- x_max / N =

						do iii=N,1,-1
							x=x_max*iii/N	!	x[0:6] <=> z[0:20]
							y=4d0*x**0.5d0
							t=y/3.75d0

							if (-1d0 <= t .and. t <= 1d0) then
								I1 = y* (0.5d0 + 0.87890594*t**2d0 + 0.51498869*t**4d0 + 0.15084934*t**6d0 + 0.02658733*t**8d0 + &
									0.00301532*t**10d0 + 0.00032411*t**12d0)
								else
									I1 = dexp(y)/y**0.5d0*(0.39894228d0 - 0.03988024/t - 0.00362018/t**2d0 + 0.00163801/t**3d0 - &
										0.01031555/t**4d0 + 0.02282967/t**5d0 - 0.02895312/t**6d0 + 0.01787654/t**7d0 - 0.00420059/t**8d0)
								endif

							W=(2d0/y*I1)**0.5d0

							if ( dabs(z+1d0-W) <= (1d0+z)*x_max/N ) exit	!if (dabs(z+1d0-W)<1d-3) YW=W-1d0

							enddo
						YW=x ; !	write(*,*) z,x
						endif
				end function

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

			real(8) function IntHwCDM(z,w,Om,Ok)
            integer i,N
				real(8) z,w,Om,Ok,Ow
					E=1d0; N=1d4 ; IntHwCDM=0d0 ;  	!= N=1d4 +- 1Mpc =1

					if (z.ne.0) then
						dz = z/N ; Ow = 1d0 - Om + Ok;
						do i=1,N ; t = dz*(i-0.5d0) + 1d0
							!if (Om.ne.1d0)
							E=( Ow*t**( 3d0 + w*3d0 ) + Om*t*t*t - Ok*t*t )**0.5d0	!
							IntHwCDM = IntHwCDM + dz/E
							enddo
						endif
					end function IntHwCDM

			real(8) function RzwCDM(z,w,Om,Ok)
				real(8) z,w,Om,Ok

					RzwCDM=0d0 ; N=1d4	!=- N=1d4 +- 1Mpc

					if ( Ok == 0d0 ) then
						RzwCDM = IntHwCDM(z,w,Om,Ok);
						else
							if (Ok > 0d0) then
								RzwCDM = dsin( dabs(Ok)**0.5d0 * IntHwCDM(z,w,Om,Ok)  ) / dabs(Ok)**0.5d0
								else
									RzwCDM = dsinh( dabs(Ok)**0.5d0 * IntHwCDM(z,w,Om,Ok)  ) / dabs(Ok)**0.5d0
								end if
						end if
				end function RzwCDM

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

		end module
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- truncated=136-=1
