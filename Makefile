# building and paking formica

all: eng

clean:
	rm -f plt/*.plt
	# rm -f doc/*/*.html
	# rm -f doc/*/*.js
	# rm -f doc/*/*.css
	rm -f *.*~ */*.*~ */*/*.*~
	rm -f *.bak */*.bak */*/*.bak
	find -name compiled | xargs rm -fr

pack: pack-eng pack-rus

pack-eng: clean
	# Packing english version
	cat plt/info-eng.rkt > info.rkt
	rm -f plt/*.plt
	cd ..; \
raco pack --plt-name "Formica 1.0" --replace --at-plt ++setup formica formica.plt formica formica/doc/eng; \
mv -f formica.plt formica/plt/; \
cd formica

pack-rus: clean
	# Packing russian version
	cat plt/info-rus.rkt > info.rkt
	rm -f plt/*.plt
	cd ..; \
raco pack --plt-name "Formica 1.0" --replace --at-plt ++setup formica formica-rus.plt formica formica/doc/rus formica/doc/FLPBook/FLPBook.pdf formica/doc/FLPBook/FLPBook.png; \
mv -f formica-rus.plt formica/plt/; \
cd formica

eng:
	# Making english version
	cat plt/info-eng.rkt > info.rkt
	raco setup -p formica && raco setup -U

rus: 
	# Making russian version
	cat plt/info-rus.rkt > info.rkt
	raco setup -p formica && raco setup -U

install:
	# liniking formica
	cd ..; raco link -l formica; cd formica
	# installing package
	raco setup -p formica && raco setup -U