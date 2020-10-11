module.exports = (grunt) => {
    grunt.initConfig({
        terser: {
            options: {
                ecma: 9,
                compress: {
                    defaults: true,
                    drop_console: true, // FIXME: Don't use console.error to display assembler errors
                    keep_fargs: false,
                    passes: 3,
                    unsafe_math: true,
                    unsafe_methods: true,
                    unsafe_proto: true,
                    unsafe_regexp: true,
                    unsafe_undefined: true
                },
                toplevel: false,
                ie8: false,
                keep_classnames: false,
                keep_fnames: true,
                safari10: false
            },
            main: {
                files: {
                    "./dist/js/hack.min.js": [
                        "js/assembler.js", "js/hack.js", "js/screen.js", "js/web.js"
                    ]
                }
            }
        },
        cssmin: {
            target: {
                files: {
                    './dist/style.min.css': [
                        'style.css'
                    ]
                }
            }
        },
    });

    grunt.loadNpmTasks('grunt-terser');
    grunt.loadNpmTasks('grunt-contrib-cssmin');

    grunt.registerTask('default', [ 'terser', "cssmin" ]);
};