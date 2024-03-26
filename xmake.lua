---@diagnostic disable: undefined-global
local fpcobj_dir = "$(buildir)/.fpcobjs/$(arch)-$(os)"
local pc_flags = {
}
if is_mode("debug") then
    pc_flags[#pc_flags+1] = "-gh"
    pc_flags[#pc_flags+1] = "-gl"
end

add_rules("mode.debug", "mode.release")
add_pcflags(unpack(pc_flags))

target("verify")
    set_kind("shared")
    if is_plat("mingw") then
        set_toolchains("zig")
    end
    add_files("./src/verify/*.cpp")

target("grab_lesson")
    set_kind("binary")
    add_deps("verify")
    set_toolchains("fpc")
    add_pcflags(
        "-Fu./src/core",
        "-Fl$(buildir)/$(plat)/$(arch)/$(mode)"
    )
    add_files("./src/grab_lesson/grab_lesson.pas")
    if is_plat("mingw") then
        add_pcflags("-Px86_64", "-Twin64")
    end

    on_load(function(target)
        local unit_target_dir = path.join(fpcobj_dir, target:name())
        if not os.exists(unit_target_dir) then
            os.mkdir(unit_target_dir)
        end
        target:add("pcflags", "-FU" .. unit_target_dir)
    end)

--
-- If you want to known more usage about xmake, please see https://xmake.io
--
-- ## FAQ
--
-- You can enter the project directory firstly before building project.
--
--   $ cd projectdir
--
-- 1. How to build project?
--
--   $ xmake
--
-- 2. How to configure project?
--
--   $ xmake f -p [macosx|linux|iphoneos ..] -a [x86_64|i386|arm64 ..] -m [debug|release]
--
-- 3. Where is the build output directory?
--
--   The default output directory is `./build` and you can configure the output directory.
--
--   $ xmake f -o outputdir
--   $ xmake
--
-- 4. How to run and debug target after building project?
--
--   $ xmake run [targetname]
--   $ xmake run -d [targetname]
--
-- 5. How to install target to the system directory or other output directory?
--
--   $ xmake install
--   $ xmake install -o installdir
--
-- 6. Add some frequently-used compilation flags in xmake.lua
--
-- @code
--    -- add debug and release modes
--    add_rules("mode.debug", "mode.release")
--
--    -- add macro definition
--    add_defines("NDEBUG", "_GNU_SOURCE=1")
--
--    -- set warning all as error
--    set_warnings("all", "error")
--
--    -- set language: c99, c++11
--    set_languages("c99", "c++11")
--
--    -- set optimization: none, faster, fastest, smallest
--    set_optimize("fastest")
--
--    -- add include search directories
--    add_includedirs("/usr/include", "/usr/local/include")
--
--    -- add link libraries and search directories
--    add_links("tbox")
--    add_linkdirs("/usr/local/lib", "/usr/lib")
--
--    -- add system link libraries
--    add_syslinks("z", "pthread")
--
--    -- add compilation and link flags
--    add_cxflags("-stdnolib", "-fno-strict-aliasing")
--    add_ldflags("-L/usr/local/lib", "-lpthread", {force = true})
--
-- @endcode
--

