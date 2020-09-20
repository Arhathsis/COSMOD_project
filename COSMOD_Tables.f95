!=- fortran-libraries
!=- © Stanislav Shirokov, 2014-2020

   module COSMOD_tables
      use global
      use GNUplot
      use cosmology
      use math

      use COSMOD_config
      use COSMOD_paths

      character(len) :: path_tab1_dat , path_tab1_tex

      integer,parameter :: range_z = 14 , range_model = 15 , range_omega_v = 5 , range_w = 1 , range_omega_k = 3

      real(8) ::  tab1_mu(range_z,1+range_model) , &
                  value_omega_v(range_omega_v) , &
                  value_w(range_w) , &
                  value_omega_k(range_omega_k) , &

                  mu_LCDM

      contains



         subroutine MN_Letter_plot_tab1
            integer w,v,k

               path_tab1_dat = trim(path_tabs) // 'MN_Letter_tab1.dat'
               path_tab1_tex = trim(path_tabs) // 'MN_Letter_tab1.tex'

               value_omega_v(1) = 0.9d0
               value_omega_v(2) = 0.7d0
               value_omega_v(3) = 0.5d0
               value_omega_v(4) = 0.3d0
               value_omega_v(5) = 0.1d0

               value_w(1) = -2

               value_omega_k(1) = -0.1d0
               value_omega_k(2) =  0.0d0
               value_omega_k(3) =  0.1d0

               tab1_mu(1 ,1) = 0.1d0
               tab1_mu(2 ,1) = 0.2d0
               tab1_mu(3 ,1) = 0.3d0
               tab1_mu(4 ,1) = 0.5d0
               tab1_mu(5 ,1) = 0.6d0
               tab1_mu(6 ,1) = 0.8d0
               tab1_mu(7 ,1) = 1.d0
               tab1_mu(8 ,1) = 2.d0
               tab1_mu(9 ,1) = 3.d0
               tab1_mu(10,1) = 4.d0
               tab1_mu(11,1) = 5.d0
               tab1_mu(12,1) = 6.d0
               tab1_mu(13,1) = 8.d0
               tab1_mu(14,1) = 10.d0

               do i=1,range_z
                  mu_LCDM = dm_wCDM( tab1_mu(i,1) , 7d1 , -1d0 , 0.3d0 , 0d0 )
                  do w=1,range_w
                     do v=1,range_omega_v
                        do k=1,range_omega_k

                           j = 1 + k + range_omega_k*(v-1) + range_omega_k*range_omega_v*(w-1)

                           tab1_mu(i,j) = dm_wCDM( tab1_mu(i,1) , 7d1 , value_w(w) , &
                              1 + value_omega_k(k) - value_omega_v(v) , value_omega_k(k)) - mu_LCDM
   write(*,*) tab1_mu(i,1) , mu_LCDM , tab1_mu(i,j) , value_w(w) , &
                              1 + value_omega_k(k) - value_omega_v(v) , value_omega_k(k) , &
                              dm_wCDM( tab1_mu(i,1) , 7d1 , value_w(w) , &
                              1 + value_omega_k(k) - value_omega_v(v) , value_omega_k(k))
                           end do
                        end do
                     end do
                  end do

               theformat = '(' // trim(inttostr(1+range_model)) // '(F5.2,1x))'
               call write_tab1(path_tab1_dat)

               theformat = '(' // trim(inttostr(range_model)) // '(F5.2,1x,"&",1x),F5.2,1x,"\\")'
               call write_tab1(path_tab1_tex)

               !call write_tab1b

            end subroutine



            subroutine write_tab1(path_table)
               character(len) path_table ; integer i

                  unit_1 = random_unit()

                  open(unit_1,file=path_table,status='replace')
                        if (theformat=='') theformat='*'
                     write(unit_1,theformat) ( tab1_mu(i,:) , i=1 , range_z)

                  close(unit_1)

               end subroutine



            subroutine write_tab1b
               integer w,v,k

                  theformat = '(i2,1x,"&",1x,F3.1,1x,"&",1x,F4.1,' // trim(inttostr(range_z)) // &
                     '(1x,"&",1x,F5.2),1x,"\\")'

                  unit_1 = random_unit()

                  open(unit_1,file=path_tab1_tex,status='replace')
                        if (theformat=='') theformat='*'

                  do w=1,range_w
                     do v=1,range_omega_v
                        do k=1,range_omega_k

                              j = 1 + k + range_omega_k*(v-1) + range_omega_k*range_omega_v*(w-1)
                           write(unit_1,theformat) int(value_w(w)) , value_omega_v(v) , value_omega_k(k) , &
                              tab1_mu(:,j)

                           end do
                        end do
                     end do

                     close(unit_1)

               end subroutine



   end module
