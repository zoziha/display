project (
    'display',
    'fortran',
    version: '1.0.0',
    license: 'MIT',
    meson_version: '>=0.60.0',
    default_options : [
        'buildtype=debugoptimized',
        'fortran_std=f2008',
        'default_library=both',
    ],
)

subdir('src')
subdir('example')

display_lic = files('LICENSE')
install_data(
    display_lic,
    install_dir: join_paths(get_option('prefix'), 'share', 'licenses', meson.project_name()),
)

pkg = import('pkgconfig')
pkg.generate(
    name : meson.project_name(),
    description : 'A simple display module for displaying on the screen',
    version : meson.project_version(),
    libraries : display_lib,
    subdirs : meson.project_name(),
)
