from setuptools import find_packages, setup

__AUTHOR__ = 'Eric Kaschalk'
__AUTHOR_EMAIL__ = 'ekaschalk@gmail.com'

setup(name='jedhy',
      version=1,
      description='Autocompletion and introspection tools for Hy.',
      author=__AUTHOR__,
      author_email=__AUTHOR_EMAIL__,
      maintainer=__AUTHOR__,
      maintainer_email=__AUTHOR_EMAIL__,
      url='https://github.com/ekaschalk/jedhy',
      license='MIT',
      keywords='python hy completion introspection refactoring emacs vim',
      packages=find_packages(exclude=["tests"]),

      # install_requires=[],
      # include_package_data=True,
      )
