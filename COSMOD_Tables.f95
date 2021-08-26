!=- fortran-libraries
!=- © Stanislav Shirokov, 2014-2020

   module COSMOD_tables
      use global
      use GNUplot
      use cosmology
      use math

      use COSMOD_config
      use COSMOD_paths

      integer,parameter :: range_z = 1d2

      character(len)    :: path_tab1_dat , path_tab1_tex

      real(8)           :: tab1_z(range_z) = NaN

      real(8),allocatable,dimension(:,:) :: tab1_mu

      contains

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

         subroutine mn2020b_tab1
            integer z,w,v,k, model , count_z
            real(8) mu_LCDM

               call cheking_MS_parameters
                  MS_real_model_count = MS_count_w * MS_count_O_w * MS_count_O_k

               count_z = count_noNaN(tab1_z)

            allocate(tab1_mu(count_z,1+MS_real_model_count))

               path_tab1_dat = trim(Tables_folder) // 'mn2020b_tab1.dat'
               path_tab1_tex = trim(Tables_folder) // 'mn2020b_tab1.tex'

               tab1_mu(:,1) = tab1_z(:)
               do z=1,count_z

                  mu_LCDM = dm_wCDM( tab1_mu(z,1) , 7d1 , -1d0 , 0.3d0 , 0d0 )

                  do w=1,MS_count_w
                     do v=1,MS_count_O_w
                        do k=1,MS_count_O_k

                           model = 1 + k + MS_count_O_k*(v-1) + MS_count_O_k*MS_count_O_w*(w-1)

                           tab1_mu( z , model ) = dm_wCDM( tab1_mu(z,1) , 7d1 , MS_EoS_w(w) , &
                              1 + MS_Omega_k(k) - MS_Omega_w(v) , MS_Omega_k(k)) - mu_LCDM

                           end do
                        end do
                     end do
                  end do

               theformat = '(' // trim(inttostr(1+MS_real_model_count)) // '(F5.2,1x))'
               call write_tab1(path_tab1_dat)

               theformat = '(' // trim(inttostr(MS_real_model_count)) // '(F5.2,1x,"&",1x),F5.2,1x,"\\")'
               call write_tab1(path_tab1_tex)

               deallocate(tab1_mu)
            end subroutine

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

            subroutine write_tab1(path_table)
               character(len) path_table ; integer i

                  unit_1 = random_unit()

                  open(unit_1,file=path_table,status='replace')
                        if (theformat=='') theformat='*'
                     write(unit_1,theformat) ( tab1_mu(i,:) , i=1 , size(tab1_mu(:,1)) )

                  close(unit_1)

               end subroutine

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

   end module
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- truncated=136-=1
