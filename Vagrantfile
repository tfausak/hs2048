require 'rbconfig'

# Vagrant 1.5.1 <http://www.vagrantup.com/downloads.html>
# VirtualBox 4.3.10 <https://www.virtualbox.org/wiki/Downloads>
Vagrant.require_version '>= 1.5.0'

# How many logical CPUs does the host machine have?
def cpus
    case RbConfig::CONFIG['host_os']
    when /mingw/
        Integer(`wmic cpu get /format:list`
            .lines
            .detect { |line| line =~ /\ANumberOfLogicalProcessors=\d+\Z/ }
            .split('=')
            .last)
    else
        1
    end
end

# How much RAM (in MB) does the host machine have?
def memory
    case RbConfig::CONFIG['host_os']
    when /mingw/
        Integer(`wmic memorychip get capacity`
            .lines
            .map { |line| line.to_i }
            .reduce(0) { |a, e| a + e }) / (1 << 20)
    else
        1024
    end
end

Vagrant.configure('2') do |config|
    config.vm.box = 'chef/ubuntu-13.10'
    config.vm.box_version = '~> 1.0'

    config.vm.provider :virtualbox do |vb|
        vb.customize({
            'modifyvm' => :id,
            '--cpus' => cpus,
            '--memory' => memory / 2
        }.to_a.flatten)
    end

    config.vm.provision :shell, inline: <<-'SH'
        set -e -x
        apt-get update
        apt-get -y install git haskell-platform haskell-platform-doc
    SH

    config.vm.provision :shell, inline: <<-'SH', privileged: false
        set -e -x
        echo 'PATH="$HOME/.cabal/bin:$PATH"' > .bash_profile
        source .bash_profile
        cabal update
        if test $(cabal --numeric-version) != '1.18.0.3'; then
            cabal install cabal-install-1.18.0.3
        fi
        for package in hlint pointfree pointful scan stylish-haskell; do
            which $package || cabal install $package
        done
    SH
end
