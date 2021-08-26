!=- Fractal Dimension Estimation
!=- © Stanislav Shirokov, 2014-2020

   module COSMOD_scripts
      use math
      use GNUplot
      use cosmology

      use COSMOD_config
      use COSMOD_paths
      use COSMOD_graphics
      use COSMOD_tables
      use COSMOD_catalogs
      use COSMOD_overleaf

      contains

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

         subroutine example

               !=- models table
               MS_table_name = trim(Models_folder) // 'example.dat'

               !=- models parameters
               MS_model(1)    = 'wCDM'
               MS_model(2)    = 'FF'

               MS_Omega_k(1)  = -0.1
               MS_Omega_k(2)  =  0.0
               MS_Omega_k(3)  =  0.1

               MS_EoS_w(1)    = -1
               MS_EoS_w(2)    = -2

               MS_Omega_w(1)  = 0.3
               MS_Omega_w(2)  = 0.7

               !=- calculating cosmological models
                  MS_recalculating = .true.
               call make_model_set

            end subroutine

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

         subroutine mn2021a_fig1
            integer i

            Data_catalogs(1) = 'C:\Users\Arhath\YandexDisk\Science\DATA\SN_catalogs\Pantheon.dat'

               columns_for_approx = '6 3 5'
                  approx_order = 8
               call approx_polylog(Data_catalogs(1))
               call plot_mn2021a_fig1(Data_catalogs(1))

            end subroutine

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

         subroutine mn2020b_overleaf

            paper_name        = 'mn2020b'

            overleaf_CanEdit  = 'https://www.overleaf.com/5957249232dfnmwjsjvdng'
            overleaf_ReadOnly = 'https://www.overleaf.com/read/ybwtxspzsdtb'

                files( 1  ) = 'Models\plots\mn2020b_fig1\mn2020b_fig1_delta_mu(z)_fig1a.eps'
            new_files( 1  ) = 'Shirokov_fig1a.eps'

                files( 2  ) = 'Models\plots\mn2020b_fig1\mn2020b_fig1_delta_mu(z)_fig1b.eps'
            new_files( 2  ) = 'Shirokov_fig1b.eps'

               call overleaf_making

            end subroutine

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

         subroutine mn2020b_plot_tab1

               MS_model(1) = 'wCDM'
               MS_Hubble_0(1) = 70

               MS_Omega_w(1) = 0.3d0
               MS_Omega_w(2) = 0.5d0
               MS_Omega_w(3) = 0.7d0
               MS_Omega_w(4) = 0.9d0

               MS_EoS_w(1) = -2

               MS_Omega_k(1) = -0.1d0
               MS_Omega_k(2) =  0.0d0
               MS_Omega_k(3) =  0.1d0

               tab1_z(1 ) = 0.1d0
               tab1_z(2 ) = 0.2d0
               tab1_z(3 ) = 0.3d0
               tab1_z(4 ) = 0.5d0
               tab1_z(5 ) = 0.6d0
               tab1_z(6 ) = 0.8d0
               tab1_z(7 ) = 1.d0
               tab1_z(8 ) = 2.d0
               tab1_z(9 ) = 3.d0
               tab1_z(10) = 4.d0
               tab1_z(11) = 5.d0
               tab1_z(12) = 6.d0
               tab1_z(13) = 8.d0
               tab1_z(14) = 10.d0

               call mn2020b_tab1

            end subroutine mn2020b_plot_tab1

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

         subroutine mn2020b_plot_fig1

               !=- models table
               MS_table_name = trim(Models_folder) // 'mn2020b_fig1.dat'

               !=- the second way setting Model_Set parameters
               !=- models parameters
               MS_model(1)    = 'wCDM'

               MS_Omega_k(1)  =  0.0   !=- first is LCDM
               MS_Omega_k(2)  = -0.1
               MS_Omega_k(3)  =  0.1

               MS_EoS_w(1)    = -1   !=- first is LCDM
               MS_EoS_w(2)    = -2

               MS_Omega_w(1)  = 0.7   !=- first is LCDM
               MS_Omega_w(2)  = 0.3

               !=- calculating cosmological models
                  !=- MS_recalculating = .true.
               call make_model_set

               !=- plotting figures
                  GRB_medians_flag = .false.
               call mn2020b_fig1
                  GRB_medians_flag = .true.
               call mn2020b_fig1

            end subroutine

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

         subroutine mn2020a_overleaf

            paper_name        = 'mn2020a'

            overleaf_CanEdit  = 'https://www.overleaf.com/9181497425tkqhjdzzmbgg'
            overleaf_ReadOnly = 'https://www.overleaf.com/read/jjrmkfbhhftv'

                files( 1  ) = 'Models\plots\mn2020a_fig3\mn2020a_fig3_r(z)-logscale.eps'
            new_files( 1  ) = 'Shirokov_fig3a.eps'

                files( 2  ) = 'Models\plots\mn2020a_fig3\mn2020a_fig3_delta_r(z).eps'
            new_files( 2  ) = 'Shirokov_fig3b.eps'

                files( 3  ) = 'Models\plots\mn2020a_fig3\mn2020a_fig3_d_L(z)-logscale.eps'
            new_files( 3  ) = 'Shirokov_fig3c.eps'

                files( 4  ) = 'Models\plots\mn2020a_fig3\mn2020a_fig3_delta_d_L(z).eps'
            new_files( 4  ) = 'Shirokov_fig3d.eps'

                files( 5  ) = 'Models\plots\mn2020a_fig3\mn2020a_fig3_mu(z)-logscale.eps'
            new_files( 5  ) = 'Shirokov_fig4a.eps'

                files( 6  ) = 'Models\plots\mn2020a_fig3\mn2020a_fig3_delta_mu(z).eps'
            new_files( 6  ) = 'Shirokov_fig4b.eps'

               call overleaf_making

            end subroutine

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

         subroutine mn2020a_plot_fig3

               !=- models table
               MS_table_name = trim(Models_folder) // 'mn2020a_fig3.dat'

            !=-   write(*,*) ' the model_set format:'
            !=-   write(*,*) ' wCDM H_0  w  Omega_v  Omega_k'
            !=-   write(*,*) ' FF H_0'
            !=-   write(*,*) ' TL H_0'

               !=- the first way setting Model_Set parameters
               model_set ( 1 , 1 ) = ' wCDM  70 -1    0.7   0.0 '
               model_set ( 1 , 2 ) = ' {/Symbol L}CDM( {/Symbol W}_{/Symbol L} = 0.7 ) '

               model_set ( 2 , 1 ) = ' wCDM  70 -1    1.0   0.0 '
               model_set ( 2 , 2 ) = ' {/Symbol L}CDM( {/Symbol W}_{/Symbol L} = 1.0 ), CSS '

               model_set ( 3 , 1 ) = ' TL    70 '
               model_set ( 3 , 2 ) = ' the TL model '

               model_set ( 4 , 1 ) = ' FF    70  '
               model_set ( 4 , 2 ) = ' the FF model '

               model_set ( 5 , 1 ) = ' wCDM  70 -1    0.9   0.0 '
               model_set ( 5 , 2 ) = ' {/Symbol L}CDM( {/Symbol W}_{/Symbol L} = 0.9 ) '

               model_set ( 6 , 1 ) = ' wCDM  70 -0.5  0.7   0.2 '
               model_set ( 6 , 2 ) = ' wCDM( w = -0.5, {/Symbol W}_{/Symbol L} = 0.7, {/Symbol W}_k = 0.2 ) '

               call make_model_set

            end subroutine mn2020a_plot_fig3

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

end module COSMOD_scripts
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- truncated=136-=1
