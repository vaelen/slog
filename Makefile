qso-report: qso-report.scm *.scm
	csc -fO -static qso-report.scm

clean:
	rm qso-report
