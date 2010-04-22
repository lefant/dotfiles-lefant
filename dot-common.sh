
find_best () {
    for p in $@
    do
        if [ $( which $p ) ]
        then
            echo $p
            return 0
        fi
    done
}

launch_all () {
    for app in $@
    do
        sh -c "sleep 0.1 && $app" &
    done
}
