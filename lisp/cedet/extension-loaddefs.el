;;; extension-loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "cedet-android" "cedet-android.el" (0 0 0 0))
;;; Generated autoloads from cedet-android.el

(autoload 'cedet-android-create-project "cedet-android" "\
Create an android project with NAME.
Your activity class will be created in the java PACKAGE.
You need to specify a TARGET, which is a number specifying the desired type
of package you intend to build.
Create the project in optional DIR, or in the default directory if not specified.
NAME will be used as the name of the project.

\(fn NAME PACKAGE TARGET &optional DIR)" t nil)

(autoload 'cedet-android-target-list "cedet-android" "\
Get the list of available targets for an android environment.

\(fn)" t nil)

(autoload 'cedet-android-layoutopt "cedet-android" "\
Get the list of available targets for an android environment.
Argument PROJECTROOT is the directory root of some project to be optimized.

\(fn PROJECTROOT)" t nil)

(autoload 'cedet-android-start-ddms "cedet-android" "\
Start Android's ddms debugging proxy.

\(fn)" t nil)

(autoload 'cedet-android-adb-help "cedet-android" "\
Get help for Android Debug Bridge.

\(fn)" t nil)

(autoload 'cedet-android-adb-devices "cedet-android" "\
The the list of attached devices from Android Debug Bridge.

\(fn)" t nil)

(autoload 'cedet-android-adb-version-check "cedet-android" "\
Check the version of the installed Android ADB command.
If optional programatic argument NOERROR is non-nil, then
instead of throwing an error if Global isn't available, then
return nil.

\(fn &optional NOERROR)" t nil)

(autoload 'cedet-android-adb-shell "cedet-android" "\
Create an inferior shell for Android Debug Bridge.

\(fn)" t nil)

(autoload 'cedet-android-sdk-update-classpath "cedet-android" "\
Update the classpath for `cedet-java' to include the android compile-time libraries.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "cedet-android" '("cedet-android-")))

;;;***

;;;### (autoloads nil "cedet-java" "cedet-java.el" (0 0 0 0))
;;; Generated autoloads from cedet-java.el

(autoload 'cedet-java-version-check "cedet-java" "\
Check the version of the installed java command.
If optional programatic argument NOERROR is non-nil, then
instead of throwing an error if Global isn't available, then
return nil.

\(fn &optional NOERROR)" t nil)

(autoload 'cedet-javap-dump-class "cedet-java" "\
Dump out a Java signatures for CLASS.
Display in a javap output buffer.

\(fn CLASS)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "cedet-java" '("cedet-ja")))

;;;***

(provide 'extension-loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; extension-loaddefs.el ends here
