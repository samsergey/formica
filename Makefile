# building and paking formica

all: eng

pack: pack-eng pack-rus

pack-eng: eng
	# Packing english version
	cd ..; \
raco pack --plt-name "Formica 1.0" --replace --at-plt ++setup formica formica.plt formica formica/doc; \
mv -f formica.plt formica/plt/; \
cd formica

pack-rus: rus
	# Packing russian version
	cd ..; \
raco pack --plt-name "Formica 1.0" --replace --at-plt ++setup formica formica-rus.plt formica formica/doc; \
mv -f formica-rus.plt formica/plt/; \
cd formica

eng:
	# Making english version
	cat plt/info-eng.rkt > info.rkt
	raco setup

rus: 
	# Making russian version
	cat plt/info-rus.rkt > info.rkt
	raco setup