README
===========

This repo is about a small application written to help make the Example Metadata Files associated with each and every Example in Lazarus Main and, optionally, in any third party packages. These metadata files are simple JSON but writing one by hand does contain some risks. This app will help you write it, or, if you have done so already, help you test that you have got it right.

Further, new in ver 1.05, it will, if you ask nicely, check, edit or set the necessary PackageFile (.lpk) entry so that Lazarus can find your example in a Third Party Package.

**Remember, three things are required for Lazarus to find an Example in your Third Party Package -** 
 * The Example's directory contains a suitable Metadata File.
 * The PackageFile (.lpk) contains an entry, eg `ExamplesDirectory Value="../demo"/` that points to the example directory relative to the .lpk file.
 * The Package is installed in Lazarus (or has been 'used' at some stage.

  This little app will help with the first two requirements. See the **Lazarus Wiki page for details **- https://wiki.freepascal.org/index.php?title=Lazarus_Examples_Window

Download
--------

Its a rough and ready little app, sure needs some polish, so, don't be afraid to report problems, make suggestions or submit a pull request.

In the Releases section, over to the right of screen, you will find pre compiled binaries for 64bit Linux and Windows, download and extract. The source should build fine on other platforms with a reasonably current Lazarus but not yet tested. 

No i18n, I doubt it will get anywhere near enough use to justify the effort but, as we know, any Lazarus Project is easy to i18n so, don't be afraid to ask.

Usage
------

In use, put the binary somewhere on your path (Sorry, I have not bothered to make install packages) and either start it and click 'New' to browse to your package example. Fill in the details, save, click "Check LPK File". It will suggest an (likely correct but check) setting and make it so if you click OK.

If your Third Pary Package has several Examples, each needs to be in its own directory, under a directory named, eg, "demos". The PackageFile entry, in this case should point to "demos", not one of the individual example directories. 

If your Example Metadata file already exists and want to edit it or create a **PackageFile ExamplesDirectory entry**, click 'Open' or call the app on the command line from where your existing Metadata file is, or provide a path to that Metadata file on the command line, eg -

	$> exmetadata ~/Pascal/MyPackages/PackageNumberOne/demo/number-one-example.ex-meta [enter] 

Without the explicite naming of a ex-meta file, just a directory, if there is a ex-meta file there, it will be opened, else, setup to create a new one.

As always, any feedback is welcome.

Davo
