COMMENT =		RPKI relying party software

MODCABAL_STEM =		rpki-prover
MODCABAL_VERSION =	0.11.0

# Not on Hackage: the source comes from the GitHub tag, every dependency from
# Hackage as usual (cabal.inc).
SITES =			https://github.com/lolepezy/rpki-prover/archive/refs/tags/
DISTFILES =		${DISTNAME}{v${MODCABAL_VERSION}}.tar.gz
HOMEPAGE =		https://github.com/lolepezy/rpki-prover

CATEGORIES =		net

MAINTAINER =		Mikhail Puzanov <misha.puzanov@pm.me>

# BSD
PERMIT_PACKAGE =	Yes

WANTLIB =		c ffi gmp iconv lzma m pthread util z

MODULES =		devel/cabal

LIB_DEPENDS =		archivers/xz \
			converters/libiconv \
			devel/gmp \
			devel/libffi
RUN_DEPENDS =		net/rsync

NO_TEST =		Yes

# cabal builds the ~300 dependencies with -j${MAKE_JOBS}, which is 1 otherwise.
DPB_PROPERTIES =	parallel

.include "cabal.inc"

.include <bsd.port.mk>
