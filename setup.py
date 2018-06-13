# import hy
from setuptools import find_packages, setup
from setuptools.command.install import install

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

      packages=["jedhy", "jedhy"],
      package_data={
          'jedhy': ['*.hy', '__pycache__/*'],
      },

      install_requires=["toolz"],
      # include_package_data=True,
      )
