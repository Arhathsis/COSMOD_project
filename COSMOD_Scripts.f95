!=- Fractal Dimension Estimation
!=- © Stanislav Shirokov, 2014-2020

module COSMOD_scripts
   use math
	use GNUplot
	use cosmology

	use COSMOD_config
	use COSMOD_paths
	use COSMOD_graphics



      contains


         subroutine MN_Letter_plot_fig1

            integer i,j,k,k_max,i_max,j_max , i_min , w , n_model
               model_set_path = trim(WorkDir) // 'MN_letter_model_set.dat'

            !=-   write(*,*) ' the model_set format:'
            !=-   write(*,*) ' wCDM H_0  w  Omega_v  Omega_k'
            !=-   write(*,*) ' FF H_0'
            !=-   write(*,*) ' TL H_0'

            MS_parameters(1)='0.3'
            MS_parameters(2)='0.7'

            w_count = 2
            Omega_v_count = 2
            Omega_k_count = 3
               MS_real_model_count = w_count*Omega_k_count*Omega_v_count


            n_model=0
            do j=1,Omega_v_count
               do i=1,w_count
                  w=-i
                     do k=1,Omega_k_count
                        MS_O_k = -0.2 + 0.1*k
            mm=4
            if (k>1) mm=5
                        read(MS_parameters(j),*) MS_O_v
                     MS_O_m_bug = abs ( 1 + MS_O_k - MS_O_v ) !=-   the Friedmann equation

                        n_model = n_model + 1
                     model_set ( n_model , 1 ) = &
                        ' wCDM  70 ' // trim(inttostr(w)) // ' ' // &
                        trim(MS_parameters(j)) // ' ' // trim(realtostrf(MS_O_k,4,1))
                     model_set ( n_model , 2 ) = &
                        ' wCDM (w = ' // trim(inttostr(w)) //  &
                        ', {/Symbol W}_{DE}=' // trim(MS_parameters(j)) // &
                        ', {/Symbol W}_m=' // trim(realtostrf(MS_O_m_bug,4,1)) // &
                        ', {/Symbol W}_k=' // trim(realtostrf(MS_O_k,mm,1)) // ') '
                     enddo
                  enddo
               enddo

               !=- the hand input:
               !model_set

                  !=-   write(*,'(A)') model_set(:,1) ; pause

               call compute_model_set
               call write_model_set
                  GRB_medians_flag = .false.
               call MN_Letter_fig1
                  GRB_medians_flag = .true.
               call MN_Letter_fig1
               !=- call MN_Letter_example


            end subroutine MN_Letter_plot_fig1


         subroutine COSMOD_model_set
            integer i,j,k,k_max,i_max,j_max , i_min , w , n_model
               model_set_path = trim(WorkDir) // 'MN_letter_model_set.dat'

            !=-   write(*,*) ' the model_set format:'
            !=-   write(*,*) ' wCDM H_0  w  Omega_v  Omega_k'
            !=-   write(*,*) ' FF H_0'
            !=-   write(*,*) ' TL H_0'

            MS_parameters(1)='0.5'
            MS_parameters(2)='0.7'
            MS_parameters(3)='0.9'

            w_count = 2
            Omega_v_count = 3
            Omega_k_count = 3
               MS_real_model_count = w_count*Omega_k_count*Omega_v_count - 6


            n_model=0
            do j=1,Omega_v_count
               if ( j /= 2 ) then
                  i_max = 1
                  else
                     i_max = w_count
                  endif
               do i=1,i_max
                  w=-i
                  if ( j /= 2 ) w=-2
                     do k=1,Omega_k_count
                        MS_O_k = -0.2 + 0.1*k
            mm=4
            if (k>1) mm=5
                        read(MS_parameters(j),*) MS_O_v
                     MS_O_m_bug = abs ( 1 + MS_O_k - MS_O_v ) !=-   the Friedmann equation
                        !write(*,*) MS_O_m_bug ; pause
                        n_model = n_model + 1
                     model_set ( n_model , 1 ) = &
                        ' wCDM  70 ' // trim(inttostr(w)) // ' ' // &
                        trim(MS_parameters(j)) // ' ' // trim(realtostrf(MS_O_k,4,1))
                     model_set ( n_model , 2 ) = &
                        ' wCDM (w = ' // trim(inttostr(w)) //  &
                        ', {/Symbol W}_{DE}=' // trim(MS_parameters(j)) // &
                        ', {/Symbol W}_m=' // trim(realtostrf(MS_O_m_bug,4,1)) // &
                        ', {/Symbol W}_k=' // trim(realtostrf(MS_O_k,mm,1)) // ') '
                     enddo
                  enddo
               enddo

               !=- the hand input:
               !model_set

                  !=-   write(*,'(A)') model_set(:,1) ; pause

               call compute_model_set
               call write_model_set
               call MN_Letter_example

            end subroutine COSMOD_model_set



         subroutine COSMOD_model_set_all
            integer i,j,k
               model_set_path = trim(WorkDir) // 'MN_letter_' // model_set_path

            !=-   write(*,*) ' the model_set format:'
            !=-   write(*,*) ' wCDM H_0  w  Omega_v  Omega_k'
            !=-   write(*,*) ' FF H_0'
            !=-   write(*,*) ' TL H_0'

            MS_parameters(1)='0.7'
            MS_parameters(2)='0.5'
            MS_parameters(3)='0.9'

            do k=1,3
                  read(MS_parameters(k),*) MS_O_v
               do i=1,3
                     MS_O_m_bug = abs ( 1 + 0.0 - MS_O_v ) !=-   the Friedmann equation
                     !write(*,*) MS_O_m_bug ; pause
                  model_set ( 1+3*(i-1)+9*(k-1) , 1 ) = ' wCDM  70 ' // trim(inttostr(-i)) // ' ' // &
                     trim(MS_parameters(k)) // ' 0.0 '
                  model_set ( 1+3*(i-1)+9*(k-1) , 2 ) = ' the wCDM (w = ' // trim(inttostr(-i)) //  &
                     ', {/Symbol W}_{/Symbol L}=' // trim(MS_parameters(k)) // &
                     ', {/Symbol W}_m=' // trim(realtostrf(MS_O_m_bug,3,1)) // &
                     ', {/Symbol W}_k= 0.0) model '

                     MS_O_m_bug = abs ( 1 + 0.1 - MS_O_v ) !=-   the Friedmann equation
                  model_set ( 2+3*(i-1)+9*(k-1) , 1 ) = ' wCDM  70 ' // trim(inttostr(-i)) // ' ' // &
                     trim(MS_parameters(k)) // ' 0.1 '
                  model_set ( 2+3*(i-1)+9*(k-1) , 2 ) = ' the wCDM (w = ' // trim(inttostr(-i)) //  &
                     ', {/Symbol W}_{/Symbol L}=' // trim(MS_parameters(k)) // &
                     ', {/Symbol W}_m=' // trim(realtostrf(MS_O_m_bug,3,1)) // &
                     ', {/Symbol W}_k= 0.1) model '

                     MS_O_m_bug = abs ( 1 - 0.1 - MS_O_v ) !=-   the Friedmann equation
                  model_set ( 3+3*(i-1)+9*(k-1) , 1 ) = ' wCDM  70 ' // trim(inttostr(-i)) // ' ' // &
                     trim(MS_parameters(k)) // ' -0.1 '
                  model_set ( 3+3*(i-1)+9*(k-1) , 2 ) = ' the wCDM (w = ' // trim(inttostr(-i)) //  &
                     ', {/Symbol W}_{/Symbol L}=' // trim(MS_parameters(k)) // &
                     ', {/Symbol W}_m=' // trim(realtostrf(MS_O_m_bug,3,1)) // &
                     ', {/Symbol W}_k=-0.1) model '
                  enddo
               enddo

               call compute_model_set
               call write_model_set
               call MN_Letter_all

            end subroutine COSMOD_model_set_all



end module COSMOD_scripts
