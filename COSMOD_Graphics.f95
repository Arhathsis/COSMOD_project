!=- fortran-libraries
!=- © Stanislav Shirokov, 2014-2020

   module COSMOD_graphics
      use global
      use GNUplot
      use cosmology
      use math

      use COSMOD_config
      use COSMOD_paths

      character(len) caption

      contains

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

         subroutine mn2020b_fig1
            integer i,j,k,ii,jj, col , start_position , i_max , k_max , j_max , n_LCDM , n_model, GNUfields_plots_i_start
            character(len) GRB_catalog_path , GRB_medians_path

               GRB_catalog_path = 'C:\Users\Arhath\YandexDisk\Science\DATA\GRB_catalogs\GRBdata_Amati-193.dat'
               GRB_medians_path = 'C:\Users\Arhath\YandexDisk\Science\DATA\GRB_catalogs\GRB_log_medians_data.dat'

               call clear_plot_fields

               pngterm  = 'set term pngcairo font "Verdana,13" size 1920, 1080'  !=-  enhanced
               epsterm  = 'set term postscript enhanced color font "Verdana,13" size 8.5, 8.5'

               GNUfields(32) = 'set termoption dashed'

               start_position = 33
                      plot1 = 100
                     title1 = 101

               GNUplot_colors(1) = 'black'
               GNUplot_colors(2) = 'red'
               GNUplot_colors(3) = 'blue'

               i_max = count_noNaN(MS_EoS_w)
               k_max = count_noNaN(MS_Omega_k)
               j_max = count_noNaN(MS_Omega_w)

               if ( MS_real_model_count /= i_max*j_max*k_max ) write(*,*) &
                  'COSMOD_Graphics: MN_Letter_fig1: incorrect model count'

               n_LCDM = 1

               n_model = 0
               do j=1,j_max         !=- Omega_v   1-2*(j-1)*(j-3)
                  do i=1,i_max      !=- w
                     do k=1,k_max   !=- Omega_k & color
                           if (i==1) thickness = 4
                           if (i==2) thickness = 2
                           if (j==1) dash_type = 6
                           if (j==2) dash_type = 1
                           !if (k==3) thickness = 2
                           !if (i==1 .and. j==2) dash_type = 6
                           !if (i==2 .and. j==2) dash_type = 1
                           !if (i==1 .and. j==1) dash_type = 3
                           !if (i==2 .and. j==1) dash_type = 4
                        n_model = n_model + 1
                        GNUfields( start_position+n_model ) = 'set linestyle ' // &
                           trim(inttostr( n_model )) // &
                           ' lw ' //trim(inttostr( thickness )) // ' pt 7 ps 0.7 dt ' // &
                           trim(inttostr( dash_type )) // &
                           ' lc rgb "' // trim( GNUplot_colors(k) ) // '"'
                        enddo
                     enddo
                  enddo

               if (GRB_medians_flag) then
                  GNUfields( start_position + n_model + 1 ) = 'set linestyle ' // &
                     trim(inttostr( n_model + 1 )) // &
                     ' lw 2 pt 7 ps 0.9 dt 1 lc rgb "dark-green"'
                  GNUfields( plot1+MS_real_model_count*2  ) = '"' // trim(slashfix(GRB_medians_path)) // &
                     '" u 1:5:3 w yerrorb ls ' // trim(inttostr( n_model + 1 ))
                  GNUfields( title1+MS_real_model_count*2  ) = ' title "  GRB_{medians}(Amati-193)"'
               endif

               GNUfields(logscale)  =  'set logscale x'
               GNUfields(xrange)    =  '[0.1:10]'

               GNUfields(legend)    =  'top left'
               GNUfields(xlabel)    =  'redshift z'
               GNUfields(grid)      =  '#xtics ytics mxtics mytics'
               GNUfields(mxtics)    =  '10'
               GNUfields(mytics)    =  '5'

               MS_fields(1) = '{/Symbol m}(z)'

               MS_fields(2) = '{/Symbol D}{/Symbol m}(z)'
               !MS_fields(3) = '{/Symbol m}(z) - {/Symbol m}_{model-' // trim(inttostr(n_LCDM)) // '}(z)'
               MS_fields(3) = '{/Symbol m}(z) - {/Symbol m}_{{/Symbol L}CDM}(z)'
               MS_fields(4) = 'mu(z)_'
               MS_fields(5) = 'delta_mu(z)_'
               MS_fields(6) = '{/Symbol D}{/Symbol m}(z)_'

               !MS_fields(2) = 'log_{10} ( 1 + {/Symbol D}{/Symbol m}(z) )'                     !=- log10
               !MS_fields(3) = 'log_{10} ( 1 + {/Symbol m}(z) - {/Symbol m}_{model-1}(z) )'   !=- log10
               !MS_fields(6) = 'log_{10} 1+{/Symbol D}{/Symbol m}(z)_'                           !=- log10

               do k=2,2    !=- k=1 - absolutes, k=2 - residuals
                  do j=3,3

                     GNUfields(title)  = trim(MS_fields(1+5*(k-1))) //' for different models'

                     do i=1,MS_real_model_count
                        col = 1+j+3*(i-1)

                        select case (k)
                           case (1)
                              GNUfields(ylabel) = trim(MS_fields(1))
                              GNUfields(yrange) =  '[38:*]'

                              GNUfields (plot1+(i-1)*2) = ', "' // trim(slashfix(MS_table_name)) // &
                                 '" u 1:' // trim( inttostr( col ) ) // ' w l ls ' // trim(inttostr(i))
                           case(2)
                              !=- GNUfields(yrange) =  '[*:*]'
                              GNUfields(yrange) =  '[-1:1.4]'
                              if (GRB_medians_flag) GNUfields(yrange) =  '[-2.3:1.4]'

                              GNUfields(ylabel) = trim(MS_fields(2)) // ' = ' // &
                                 trim(MS_fields(3))

                              GNUfields (plot1+(i-1)*2) = '"' // trim(slashfix(MS_table_name)) // &
                                 '" u 1:(($' // trim( inttostr( col ) ) // ' - $' // &
                                 !'" u 1:(log10(10+$' // trim( inttostr( col ) ) // ' - $' // &   !=- log10
                                 trim(inttostr( 1+j+3*(n_LCDM-1) )) // ')) w l ls ' // trim(inttostr(i))  !=- log10
                           end select

                        !=- GNUfields (title1+(i-1)*2) = 'model-' // trim(adjustl(inttostr(i))) // ':' // &   !=-
                        GNUfields (title1+(i-1)*2) =  &   !=-
                           trim( model_set (i,2) )
                        if (GRB_medians_flag) GNUfields(title1+(i-1)*2) = 'notitle'
                        end do


                        if (GRB_medians_flag) then
                           caption='fig1b'
                           else
                              caption='fig1a'
                           endif
                     graph_name = trim(MS_fields(4+1*(k-1))) // trim(caption)
                        GNUfields(extention_out_figure)='#eps' ; call plot(MS_table_name)
                     graph_name = trim(MS_fields(4+1*(k-1))) // trim(caption)
                        GNUfields(extention_out_figure)='#png' ; call plot(MS_table_name)
                     enddo
                  enddo

            end subroutine mn2020b_fig1

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

         subroutine plot_mn2021a_fig1(source_data)
            character(len) source_data
            character(2) :: delta_column = '7'

               call clear_plot_fields

               graph_name = trim(adjustl(inttostr(approx_order))) // '_' // trim(name(source_data))

               pngterm  = 'set term pngcairo enhanced font "Verdana,20" size 1400, 1050'
               epsterm  = 'set term postscript enhanced color font "Verdana,14" size 8.5, 6.3'

               GNUfields (legend)   =  'right bottom'

               GNUfields (title)    =  ' approximation of the ' // trim(name(source_data)) // &
                  ' data (CosMod v1.0)'
               GNUfields (xlabel)   =  'z'
               GNUfields (ylabel)   =  '{/Symbol m}'
               GNUfields (logscale) =  'set logscale x'

               GNUfields (ls1)      =  'set linestyle 1 lw 1 pt 7 ps 0.7 lt rgb "blue"'
               GNUfields (ls2)      =  'set linestyle 2 lw 2 pt 7 ps 0.7 lt rgb "black"'
               GNUfields (ls3)      =  'set linestyle 3 lw 4 pt 7 ps 0.7 lt rgb "yellow"'
               GNUfields (ls4)      =  'set linestyle 4 lw 3 pt 7 ps 0.7 lt rgb "red"'

               GNUfields (plot1)    =  '"' // trim((approx_datafile)) // '" u 1:2:4 w yerrorb ls 1'
               GNUfields (plot2)    =  '"' // trim((approx_datafile)) // '" u 1:5   w l ls 3'
               GNUfields (plot3)    =  '"' // trim((approx_datafile)) // '" u 1:6   w l ls 2'
               GNUfields (plot4)    =  '"' // trim((approx_datafile)) // '" u 1:7   w l ls 4'

               GNUfields (title1)   =  'the ' // trim(name(source_data)) // ' data'
               GNUfields (title2)   =  'polylog approximation power ' // trim( adjustl( inttostr(approx_order) ) )
               GNUfields (title3)   =  'first linear approximation (FLA)'
               GNUfields (title4)   =  'LCDM (70,0.7)'

               GNUfields(extention_out_figure)='png' ; call plot(approx_datafile)

               graph_name = 'delta_LCDM_' // trim(adjustl(inttostr(approx_order))) // '_' // trim(name(source_data))

               GNUfields (ylabel)   =  '{/Symbol D}{/Symbol m} = {/Symbol m} - {/Symbol m}_{LCDM(70,0.7)}'

                  delta_column = '7'
               GNUfields (plot1)    =  '"' // trim((approx_datafile)) // '" u 1:($2-$' // delta_column // '):4 w yerrorb ls 1'
               GNUfields (plot2)    =  '"' // trim((approx_datafile)) // '" u 1:($5-$' // delta_column // ')   w l ls 3'
               GNUfields (plot3)    =  '"' // trim((approx_datafile)) // '" u 1:($6-$' // delta_column // ')   w l ls 2'
               GNUfields (plot4)    =  '"' // trim((approx_datafile)) // '" u 1:($7-$' // delta_column // ')   w l ls 4'

               GNUfields(extention_out_figure)='png' ; call plot(approx_datafile)

               graph_name = 'delta_linear_' // trim(adjustl(inttostr(approx_order))) // '_' // trim(name(source_data))

               GNUfields (ylabel)   =  '{/Symbol D}{/Symbol m} = {/Symbol m} - {/Symbol m}_{FLA}'

                  delta_column = '6'
               GNUfields (plot1)    =  '"' // trim((approx_datafile)) // '" u 1:($2-$' // delta_column // '):4 w yerrorb ls 1'
               GNUfields (plot2)    =  '"' // trim((approx_datafile)) // '" u 1:($5-$' // delta_column // ')   w l ls 3'
               GNUfields (plot3)    =  '"' // trim((approx_datafile)) // '" u 1:($6-$' // delta_column // ')   w l ls 2'
               GNUfields (plot4)    =  '"' // trim((approx_datafile)) // '" u 1:($7-$' // delta_column // ')   w l ls 4'

               GNUfields(extention_out_figure)='png' ; call plot(approx_datafile)

               graph_name = '5log_x_' // trim(adjustl(inttostr(approx_order))) // '_' // trim(name(source_data))

               GNUfields (ylabel)   =  '{/Symbol D}{/Symbol m} = {/Symbol m} - {/Symbol m}_{55+5log x}'

                  delta_column = '8'
               GNUfields (plot1)    =  '"' // trim((approx_datafile)) // '" u 1:($2-$' // delta_column // '):4 w yerrorb ls 1'
               GNUfields (plot2)    =  '"' // trim((approx_datafile)) // '" u 1:($5-$' // delta_column // ')   w l ls 3'
               GNUfields (plot3)    =  '"' // trim((approx_datafile)) // '" u 1:($6-$' // delta_column // ')   w l ls 2'
               GNUfields (plot4)    =  '"' // trim((approx_datafile)) // '" u 1:($7-$' // delta_column // ')   w l ls 4'

               GNUfields(extention_out_figure)='png' ; call plot(approx_datafile)

            end subroutine

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

end module COSMOD_graphics
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- truncated=136-=1
