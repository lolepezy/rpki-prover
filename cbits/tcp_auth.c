/*
 * TCP-MD5 (RFC 2385) for listening sockets, see RPKI.RTR.TcpAuth.
 *
 * Linux takes the keys themselves as a socket option: a key is set on the
 * listening socket for an address prefix of the peers, accepted connections
 * inherit the key that matches the peer and the kernel then drops every
 * segment from that peer that isn't signed with it.
 *
 * OpenBSD keeps the keys in the IPsec SA database instead, as SAs of type
 * tcpmd5 per pair of addresses (ipsec.conf(5), ipsecctl(8)), and the socket
 * option is only a flag that turns signatures on. On a listening socket with
 * the flag, a SYN from a peer without an SA is accepted unsigned, a SYN from
 * a peer with one must be signed with its key, and only the connections that
 * were signed have the flag after accept (tcp_dooptions, syn_cache_get).
 *
 * The other BSDs look similar, but are not checked, and macOS has no TCP-MD5.
 */

#include <errno.h>
#include <string.h>

/* Keep in sync with RPKI.RTR.TcpAuth */
#define TCP_MD5_UNSUPPORTED 0
#define TCP_MD5_SOCKET_KEYS 1
#define TCP_MD5_IPSEC_SA    2

#if defined(__linux__)

#include <stdint.h>
#include <netinet/in.h>
#include <sys/socket.h>

/*
 * struct tcp_md5sig from linux/tcp.h. It's the kernel ABI, so it's defined
 * here not to depend on whether the libc headers (glibc or musl, and of which
 * version) have the extended version of it.
 */
struct rpki_prover_tcp_md5sig {
    struct sockaddr_storage addr;
    uint8_t  flags;
    uint8_t  prefixlen;
    uint16_t keylen;
    int      ifindex;
    uint8_t  key[80];
};

/* Since Linux 4.13 */
#define RPKI_PROVER_TCP_MD5SIG_EXT         32
#define RPKI_PROVER_TCP_MD5SIG_FLAG_PREFIX 1

int rpki_prover_tcp_md5_support(void)
{
    return TCP_MD5_SOCKET_KEYS;
}

/*
 * Set the key for the peers in addr/prefixlen. The family is that of the
 * socket, addr is in network byte order, 4 bytes for AF_INET and 16 for
 * AF_INET6. An IPv4 prefix on an AF_INET6 socket is given as an IPv4-mapped
 * address with the prefix length of the IPv4 prefix.
 *
 * Returns 0 or -1 with errno set.
 */
int rpki_prover_set_tcp_md5_key(int fd, int family, const uint8_t *addr,
                                int prefixlen, const uint8_t *key, int keylen)
{
    struct rpki_prover_tcp_md5sig sig;

    if (keylen < 0 || (size_t) keylen > sizeof(sig.key)) {
        errno = EINVAL;
        return -1;
    }

    memset(&sig, 0, sizeof(sig));

    if (family == AF_INET) {
        struct sockaddr_in *sin = (struct sockaddr_in *) &sig.addr;
        sin->sin_family = AF_INET;
        memcpy(&sin->sin_addr, addr, 4);
    } else if (family == AF_INET6) {
        struct sockaddr_in6 *sin6 = (struct sockaddr_in6 *) &sig.addr;
        sin6->sin6_family = AF_INET6;
        memcpy(&sin6->sin6_addr, addr, 16);
    } else {
        errno = EAFNOSUPPORT;
        return -1;
    }

    sig.flags     = RPKI_PROVER_TCP_MD5SIG_FLAG_PREFIX;
    sig.prefixlen = (uint8_t) prefixlen;
    sig.keylen    = (uint16_t) keylen;
    memcpy(sig.key, key, keylen);

    return setsockopt(fd, IPPROTO_TCP, RPKI_PROVER_TCP_MD5SIG_EXT, &sig, sizeof(sig));
}

#else

int rpki_prover_set_tcp_md5_key(int fd, int family, const unsigned char *addr,
                                int prefixlen, const unsigned char *key, int keylen)
{
    (void) fd; (void) family; (void) addr; (void) prefixlen; (void) key; (void) keylen;
    errno = ENOPROTOOPT;
    return -1;
}

#endif

#if defined(__OpenBSD__)

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>

int rpki_prover_tcp_md5_support(void)
{
    return TCP_MD5_IPSEC_SA;
}

/*
 * Turn signatures on for a listening socket, before listen(). Fails with
 * ENOPROTOOPT if the kernel is built without TCP_SIGNATURE.
 */
int rpki_prover_enable_tcp_md5(int fd)
{
    int on = 1;
    return setsockopt(fd, IPPROTO_TCP, TCP_MD5SIG, &on, sizeof(on));
}

/* 1 if the segments of an accepted connection are signed, 0 if not, -1 on error */
int rpki_prover_tcp_md5_enabled(int fd)
{
    int on = 0;
    socklen_t len = sizeof(on);
    if (getsockopt(fd, IPPROTO_TCP, TCP_MD5SIG, &on, &len) == -1)
        return -1;
    return on != 0;
}

#else

#if !defined(__linux__)
int rpki_prover_tcp_md5_support(void)
{
    return TCP_MD5_UNSUPPORTED;
}
#endif

int rpki_prover_enable_tcp_md5(int fd)
{
    (void) fd;
    errno = ENOPROTOOPT;
    return -1;
}

int rpki_prover_tcp_md5_enabled(int fd)
{
    (void) fd;
    errno = ENOPROTOOPT;
    return -1;
}

#endif
