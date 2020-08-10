library(LanguageToolR)

if (!lato_test_setup()) {
  current_executable <- lato_quick_setup(path = tempdir())
} else {
  current_executable <- lato_get_default_executable()
}

# show executable path
print(current_executable)

# show version
print(lato_get_version(executable = current_executable))
