<?php
/*************************************************************************************
 * efene.php
 * --------
 * Author: Mariano Guerra (luismarianoguerra@gmail.com)
 * Copyright: (c) 2010 Mariano Guerra (http://marianoguerra.com.ar)
 * Release Version: v.v.v.v
 * Date Started: 2010/04/13
 *
 * efene language file for GeSHi.
 *
 * CHANGES
 * -------
 * yyyy/mm/dd (v.v.v.v)
 *  -  First Release
 *
 * TODO (updated yyyy/mm/dd)
 * -------------------------
 *
 *
 *************************************************************************************
 *
 *     This file is part of GeSHi.
 *
 *   GeSHi is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   GeSHi is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with GeSHi; if not, write to the Free Software
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 ************************************************************************************/

$language_data = array(
    'LANG_NAME' => 'efene',
    'COMMENT_SINGLE' => array(1 => '#'),
    'COMMENT_MULTI' => array(),
    'CASE_KEYWORDS' => GESHI_CAPS_NO_CHANGE,
    'QUOTEMARKS' => array('"', '\''),
    'ESCAPE_CHAR' => '\\',
    'KEYWORDS' => array(1 => array('if', 'else', 'for', 'in', 'receive', 'after', 'switch', 'case', 'break', 'when', 'try', 'catch', 'object'), 2 => array('true', 'false'), 3 => array('and', 'or', 'andd', 'orr', 'xor', 'not'), 4 => array('and', 'or', 'andd', 'orr', 'xor', 'not')
        ),
    'SYMBOLS' => array(
        1 => array(
            '(', ')', '{', '}', '[', ']', '+', '-', '*', '/', '%', '=', '<', '>', '!', '^', '&', '|', '?', ':', ';', ','
            )
        ),
    'CASE_SENSITIVE' => array(
        GESHI_COMMENTS => false,
        1 => true, 2 => true, 3 => true, 4 => true
        ),
    'STYLES' => array(
        'KEYWORDS' => array(1 => 'color: #b1b100;', 2 => 'color: #b1b100;', 3 => 'color: #b1b100;', 4 => 'color: #b1b100;'
            ),
        'COMMENTS' => array(
            1 => 'color: #666666; font-style: italic;',
            'MULTI' => 'color: #666666; font-style: italic;'
            ),
        'ESCAPE_CHAR' => array(
            0 => 'color: #000099; font-weight: bold;'
            ),
        'BRACKETS' => array(
            0 => 'color: #009900;'
            ),
        'STRINGS' => array(
            0 => 'color: #0000ff;'
            ),
        'NUMBERS' => array(
            0 => 'color: #cc66cc;',
            ),
        'METHODS' => array(
            0 => 'color: #004000;'
            ),
        'SYMBOLS' => array(
            1 => 'color: #339933;'
            ),
        'REGEXPS' => array(),
        'SCRIPT' => array()
        ),
    'URLS' => array(1 => '', 2 => '', 3 => '', 4 => ''),
    'OOLANG' => true,
    'OBJECT_SPLITTERS' => array(1 => '.', 2 => '-&gt;'),
    'REGEXPS' => array(),
    'STRICT_MODE_APPLIES' => GESHI_NEVER,
    'SCRIPT_DELIMITERS' => array(),
    'HIGHLIGHT_STRICT_BLOCK' => array()
);

?>
