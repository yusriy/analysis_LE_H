import cdsapi

# Initialize the CDS API client
c = cdsapi.Client()

# Define the request parameters
request_params = {
    'product_type': 'reanalysis',
    'variable': [
        'skin_temperature', 'sea_surface_temperature', '2m_temperature',
        'significant_height_of_combined_wind_waves_and_swell',
        'surface_downwelling_longwave_radiation',
        'surface_downwelling_shortwave_radiation',
        'surface_net_downward_longwave_radiation',
        'surface_net_downward_shortwave_radiation',
        'surface_latent_heat_flux', 'surface_sensible_heat_flux'
    ],
    'year': ['2022'],
    'month': ['03', '04', '05', '06', '07', '08', '09'],
    'day': [
        '01', '02', '03', '04', '05', '06', '07', '08', '09', '10',
        '11', '12', '13', '14', '15', '16', '17', '18', '19', '20',
        '21', '22', '23', '24', '25', '26', '27', '28', '29', '30', '31'
    ],
    'time': [
        '00:00', '01:00', '02:00', '03:00', '04:00', '05:00',
        '06:00', '07:00', '08:00', '09:00', '10:00', '11:00',
        '12:00', '13:00', '14:00', '15:00', '16:00', '17:00',
        '18:00', '19:00', '20:00', '21:00', '22:00', '23:00'
    ],
    'area': [
        5.75, 100,  # North, West
        5.5, 100.25  # South, East
    ],
    'grid': [0.25, 0.25],
    'format': 'netcdf',
}

# Request the data from the CDS API
c.retrieve(
    'reanalysis-era5-single-levels',
    request_params,
    'era5_data.nc'
)
