# ecos (development version)

# ecos 0.1.7

## bug fixes

* Due to changes in ECOS, the `stat_code` parameter has been added to the arguments of the `statTableList()` function.

* `ecos::statSearch()` without loading the package occurs an error regarding `getCalendarTime()` function. so I modified `calendar` to `ecos::calendar`.

# ecos 0.1.6

## bug fixes

* `statSearch()` function without stat_code shows all stat list. but the default `max.print` option is too short to show all stat list. so I modified `max.print` to `.Machine$integer.max`. 

# ecos 0.1.5

## bug fixes

* `setKey()`, `printKey()` have been replaced by `kosis.setKey()`, `kosis.printKey()`
