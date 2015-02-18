function emacsinit(clientcommand)
% EMACSINIT Initialize the current MATLAB session for matlab-shell-mode
    
    if usejava('jvm')
        % Disable built-in editor showing up for debugging
        com.mathworks.services.Prefs.setBooleanPref('EditorGraphicalDebugging', false);
        % Disable the built-in editor for normal editing
        com.mathworks.services.Prefs.setBooleanPref('EditorBuiltinEditor', false);
        % Use emacsclient no-wait to send edit requests to a
        % running emacs.
        if nargin == 0
            clientcommand = 'emacsclient -n';
        end
        com.mathworks.services.Prefs.setStringPref('EditorOtherEditor', clientcommand);
    end

    % Use the desktop hotlinking system in MATLAB Shell.  matlab-shell
    % will interpret them, and provide clickable areas.
    % NOTE: This doesn't work in all cases where HotLinks are used.
    feature('HotLinks','on');
end