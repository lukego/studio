# Studio

Studio is an interactive software diagnostics environment.

Studio imports dense and "messy" diagnostic data in an application's
own native formats, then it applies all the tools that are needed to
extract useful information, and finally it presents the results in an
interactive graphical user interface.

### Getting Started

Studio runs on Linux/x86-64. You can choose whether to run Studio natively, or on a server, or in a Docker container, or in a virtual machine, etc.

#### VNC Client

You can run a VNC client on your local machine to remotely access the
Studio GUI. We recommend choosing a VNC client that supports resizing
the desktop, for example TigerVNC.

The basic usage of a VNC client is:

```shell
vncviewer hostname:1
```

where `hostname` is the machine running Studio and VNC desktop number
1 corresponds to TCP port number 5901.

#### Docker on Linux/Windows/Mac

Docker is a convenient way to run Studio locally on a Linux, Mac, or Windows machine. Here is an example:

```shell
docker run -v /:/host --rm -ti -p 127.0.0.1:5901:5901 studioproject/studio vnc
```

This command:
- Automatically downloads the `studioproject/studio` image from Dockerhub. This image is automatically published by the Studio CI each time a new working version is pushed to the `master` branch.
- Forwards TCP port `localhost:5901` to port `5901` inside the container. This way a VNC client connecting to desktop `localhost:1` will automatically connect to Studio inside Docker.
- Mounts the host filesystem `/` under `/host` inside the container. This allows Studio to access host machine paths such as `/host/home/me/...`.
- Makes the container one-shot with automatic removal on shutdown (`--rm`).
- Prints debug output from the container to stdout (`-ti`).

Docker automatically caches the downloaded Studio image. If you want to update this to the latest `master` branch then you can pull an update like this:

```shell
docker pull studioproject/studio
```

#### Natively on Linux/x86-64

You can run Studio directly on a Linux/x86-64 machine (or virtual machine.) Here are the recommended installation steps:

```shell
curl https://nixos.org/nix/install | sh                 # Get nix package manager
nix-env -iA cachix -f https://cachix.org/api/v1/install # Install Nix cache manager
cachix use studio                                       # Enable download of cached builds
git clone https://github.com/studio/studio              # Get Studio
studio/run vnc                                          # Start GUI as VNC server
```

The use of [Cachix]() is optional but it speeds up installation.
Studio CI automatically populates the Cachix server with binary builds
of all software packages that Studio uses.

#### Optional extras

Here are a few variations on the commands above:

```shell
git checkout next         # Switch to development version
studio/run x11            # Start GUI directly as X11 client
docker build -t studio .  # Build the Docker image from scratch
```

----

<p align="center"> <img src="studio.svg" alt="Studio screenshot" width=600> <br/> RaptorJIT IR visualization example </p>

