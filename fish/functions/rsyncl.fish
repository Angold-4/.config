function rsyncl
    set directory $argv[1]
    set server_name $argv[2]
    set server_ip "192.168.50.119"

    switch $server_name
        case black
            set server_ip "192.168.50.119"
        case "*"
            echo "Unknown server name: $server_name"
            return 1
    end

    rsync -azP --exclude='target' "." angold@$server_ip:"/home/angold/rsync/$directory"
end
