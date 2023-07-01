filenames = [
    'display.F90',
]

display_lib = library(
    meson.project_name(),
    sources: files(filenames),
    version: meson.project_version(),
    install: true,
)

display_inc = display_lib.private_dir_include()
display_dep = declare_dependency(
    link_with: display_lib,
    include_directories: display_inc,
)

if host_machine.system() == 'windows'
    symbols_file = 'lib'+meson.project_name()+'-'+meson.project_version().split('.')[0]+'.dll.symbols'
    obj_suffix = '.obj'
else
    symbols_file = 'lib'+meson.project_name()+'.so.'+meson.project_version()+'.symbols'
    obj_suffix = '.o'
endif

install_subdir(
    display_lib.full_path()+'.p',
    install_dir: 'include'/meson.project_name(),
    strip_directory: true,
    exclude_files: [
        'depscan.dd',
        meson.project_name()+'-deps.json',
        symbols_file,
        meson.project_name()+'.dat',
        'display.F90'+obj_suffix,
    ],
)
