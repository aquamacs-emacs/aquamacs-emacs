function opentoline(file, line, column)
%OPENTOLINE Open to specified line in function file in Emacs.
%   This is a hack to override the built-in opentoline program in MATLAB.
%
%   Remove this M file from your path to get the old behavior.

    editor = system_dependent('getpref', 'EditorOtherEditor');
    editor = editor(2:end);
    
    if nargin==3
        linecol = sprintf('+%d:%d',line,column);
    else
        linecol = sprintf('+%d',line);
    end
    
    if ispc
        % On Windows, we need to wrap the editor command in double quotes
        % in case it contains spaces
        system(['"' editor '" "' linecol '" "' file '"&']);
    else
        % On UNIX, we don't want to use quotes in case the user's editor
        % command contains arguments (like "xterm -e vi")
        system([editor ' "' linecol '" "' file '" &']);
    end
end
