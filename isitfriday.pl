$d=(localtime time)[6];

print ($d > 0 && $d < 4 ? "Ei ole.\n" :
	$d == 4 ? "Varareede alles\n" :
	$d == 5 ? "JAH!!!\n" :
  	"Nädalavahetus on, kasi tagasi jooma...\n")
