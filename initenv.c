#include <locale.h>
#include <signal.h>

void hirc_init_env() {
	setlocale(LC_ALL, "C");
	signal(SIGPIPE, SIG_IGN);
}
